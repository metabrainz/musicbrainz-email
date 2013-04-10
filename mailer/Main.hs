{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding (catch)


--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forever, void)
import Control.Monad.Trans.Either (runEitherT)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Concurrent.MVar (newMVar, readMVar, withMVar)
import Control.Concurrent (threadDelay)

import Data.Aeson ((.=))

--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Generic as GAeson
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
-- | Takes a 'AMQP.Connection' and returns a callback that can be used on the
-- outbox queue. To form the callback, IO is performed to open a 'AMQP.Channel'
-- for the callback, so that it can publish failures.
emailConsumer :: AMQP.Connection -> IO (AMQP.Message -> AMQP.Envelope -> IO ())
emailConsumer rabbitMqConn = do
  rabbitMq <- AMQP.openChannel rabbitMqConn

  (invalidQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox.invalid" }

  (unroutableQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox.unroutable" }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = failureExchange
                     , AMQP.exchangeType = "direct"
                     }

  AMQP.bindQueue rabbitMq invalidQueue failureExchange invalidKey
  AMQP.bindQueue rabbitMq unroutableQueue failureExchange unroutableKey

  heist <- fmap (either (error . show) id) $ runEitherT $ do
      templateRepo <- Heist.loadTemplates "templates"
      Heist.initHeist (Heist.HeistConfig [] [] [] [] templateRepo)

  lastTime <- Time.getCurrentTime >>= newMVar
  let updateLastSent = void $ withMVar lastTime (const $ evaluate <$> Time.getCurrentTime)

  return $ \msg env -> do
    case GAeson.decode (AMQP.msgBody msg) of
      -- JSON decoding failed
      Nothing ->
        AMQP.publishMsg rabbitMq failureExchange invalidKey msg

      Just email -> do
        case emailToMail email heist of
          -- Template rendering failed
          Nothing ->
            AMQP.publishMsg rabbitMq failureExchange invalidKey msg

          Just mail -> do
            now <- Time.getCurrentTime
            lastSentAt <- readMVar lastTime

            threadDelay $ floor $
              (max 0 (maxRate - (now `Time.diffUTCTime` lastSentAt))) * 1000000

            (Mail.renderSendMail mail >> updateLastSent) `catch`
              (\e -> AMQP.publishMsg rabbitMq failureExchange unroutableKey
                       AMQP.newMsg
                         { AMQP.msgBody = Aeson.encode $ Aeson.object
                             [ "email" .= GAeson.encode email
                             , "error" .= Text.pack (show (e :: SomeException))
                             ]
                         })

    AMQP.ackEnv env

 where

  failureExchange = "failure"
  invalidKey = "invalid"
  unroutableKey = "unroutable"

  maxRate = 0.12342


--------------------------------------------------------------------------------
main :: IO ()
main = do
  rabbitMqConn <- AMQP.openConnection "127.0.0.1" "/email" "guest" "guest"
  rabbitMq <- AMQP.openChannel rabbitMqConn

  (outboxQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox" }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = Email.outboxExchange
                     , AMQP.exchangeType = "fanout"
                     }

  AMQP.bindQueue rabbitMq outboxQueue Email.outboxExchange ""

  consumer <- emailConsumer rabbitMqConn
  AMQP.consumeMsgs rabbitMq outboxQueue AMQP.Ack (uncurry consumer)

  void $ forever getLine
