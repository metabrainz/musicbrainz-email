{-# LANGUAGE OverloadedStrings #-}
module Mailer where

--------------------------------------------------------------------------------
import Prelude hiding (catch)


--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Concurrent.MVar (newMVar, readMVar, withMVar)
import Control.Concurrent (threadDelay)

import Control.Monad.Trans (lift)
import Data.Aeson ((.=))

--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Builder
import qualified Control.Error as Error
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Heist as Heist
import qualified Heist.Interpreted as Heist
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Text.XmlHtml as XmlHtml

--------------------------------------------------------------------------------
import qualified MusicBrainz.Email as Email


--------------------------------------------------------------------------------
emailToMail :: Email.Email -> Heist.HeistState Identity -> Maybe Mail.Mail
emailToMail email heist = runIdentity $
  Heist.evalHeistT
    mailBuilder
    (XmlHtml.TextNode "")
    (Heist.bindStrings templateBindings heist)

 where

  mailBuilder = fmap (makeMail . runTemplate) <$> Heist.evalTemplate templatePath

  runTemplate = Builder.toLazyByteString . XmlHtml.renderHtmlFragment XmlHtml.UTF8

  template = Email.emailTemplate email

  templatePath = case template of
    (Email.PasswordReset _) -> "password-reset"

  templateBindings = case template of
    (Email.PasswordReset editor) -> [("editor", editor)]

  emailSubject = case template of
    (Email.PasswordReset _) -> "Mandatory Password Reset"

  makeMail messageBody = Mail.Mail
    { Mail.mailFrom = Email.emailFrom email
    , Mail.mailTo = [ Email.emailTo email ]
    , Mail.mailCc = []
    , Mail.mailBcc = []
    , Mail.mailHeaders = [("Subject", emailSubject)]
    , Mail.mailParts = [ [ Mail.Part { Mail.partType = "text/plain"
                                     , Mail.partEncoding = Mail.None
                                     , Mail.partFilename = Nothing
                                     , Mail.partHeaders = []
                                     , Mail.partContent = messageBody
                                     } ] ]
    }


--------------------------------------------------------------------------------
-- | Given a rate, which is the maximum allowable throughput per second, this
-- will produce a rate limiting action, which when invoked will cause the
-- calling thread to sleep for the minimum duration required to adhere to the
-- requested rate limit.
rateLimiter :: Time.NominalDiffTime -> IO (IO ())
rateLimiter rate = do
  lastSent <- Time.getCurrentTime >>= newMVar

  return $ do
    now <- Time.getCurrentTime
    lastSentAt <- readMVar lastSent

    threadDelay $ floor $
      max 0 ((1 / rate) - (now `Time.diffUTCTime` lastSentAt)) * 1000000

    updateLastSent lastSent

 where
  updateLastSent lastTime = void $
    withMVar lastTime (const $ evaluate <$> Time.getCurrentTime)


--------------------------------------------------------------------------------
-- | Takes a 'AMQP.Connection' and returns a callback that can be used on the
-- outbox queue. To form the callback, IO is performed to open a 'AMQP.Channel'
-- for the callback, so that it can publish failures.
consumeOutbox :: AMQP.Connection
              -> (Mail.Mail -> IO ())
              -> IO ()
consumeOutbox rabbitMqConn sendMail = do
  rabbitMq <- AMQP.openChannel rabbitMqConn
  heist <- loadTemplates

  rateLimit <- let approximateEditorCount = 680000
                   day = 24 * 60 * 60.0
               in rateLimiter (approximateEditorCount / day)

  void $ AMQP.consumeMsgs rabbitMq Email.outboxQueue AMQP.Ack $
    \(msg, env) -> do
      maybe
        (publishFailure rabbitMq Email.invalidKey msg)
        (Error.eitherT (publishFailure rabbitMq Email.unroutableKey) return .
           (\email -> lift rateLimit >> trySendEmail heist email))
        (Aeson.decode $ AMQP.msgBody msg)

      AMQP.ackEnv env

 where

  trySendEmail heist email = tryFormEmail >>= trySend

   where

    tryFormEmail =
      let failureMessage = AMQP.newMsg { AMQP.msgBody = Aeson.encode email }
      in Error.EitherT $ return $
           Error.note failureMessage $ emailToMail email heist

    trySend mail =
      let exceptionMessage e = AMQP.newMsg
            { AMQP.msgBody = Aeson.encode $ Aeson.object
                [ "email" .= Aeson.encode email
                , "error" .= Text.pack (show (e :: SomeException))
                ]
            }
      in Error.bimapEitherT exceptionMessage id $
           Error.EitherT $ try (sendMail mail)

  publishFailure rabbitMq = AMQP.publishMsg rabbitMq Email.failureExchange


--------------------------------------------------------------------------------
loadTemplates :: Monad m => IO (Heist.HeistState m)
loadTemplates = fmap (either (error . show) id) $ Error.runEitherT $ do
    templateRepo <- Heist.loadTemplates "templates"
    Heist.initHeist (Heist.HeistConfig [] [] [] [] templateRepo)
