{-# LANGUAGE OverloadedStrings #-}
module Mailer where

--------------------------------------------------------------------------------
import Prelude hiding (catch)


--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid (mempty)

import Data.Aeson ((.=))

--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Builder
import qualified Control.Error as Error
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Heist as Heist
import qualified Heist.Interpreted as Heist
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Text.XmlHtml as XmlHtml

--------------------------------------------------------------------------------
import qualified MusicBrainz.Email as Email
import qualified Paths_musicbrainz_email as Email


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
    , Mail.mailParts = [ [ Mail.Part { Mail.partType = "text/plain; charset=UTF-8"
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
consumeOutbox :: AMQP.Connection
              -> Heist.HeistState Identity
              -> (Mail.Mail -> IO ())
              -> IO ()
consumeOutbox rabbitMqConn heist sendMail = do
  rabbitMq <- AMQP.openChannel rabbitMqConn

  void $ AMQP.consumeMsgs rabbitMq Email.outboxQueue AMQP.Ack $
    \(msg, env) -> do
      maybe
        (publishFailure rabbitMq Email.invalidKey msg)
        (Error.eitherT (publishFailure rabbitMq Email.unroutableKey) return .
           trySendEmail)
        (Aeson.decode $ AMQP.msgBody msg)

      AMQP.ackEnv env

 where

  trySendEmail email = tryFormEmail >>= trySend

   where

    failureMessage e =
      AMQP.newMsg { AMQP.msgBody = Aeson.encode $ Aeson.object
                      [ "email" .= email
                      , "error" .= (e :: Text.Text)
                      ]
                  }

    tryFormEmail =
      Error.EitherT $ return $
        Error.note (failureMessage "Couldn't render template") $
          emailToMail email heist

    trySend mail =
      let exceptionMessage e =
            failureMessage (Text.pack (show (e :: SomeException)))
      in Error.bimapEitherT exceptionMessage id $
           Error.EitherT $ try (sendMail mail)

  publishFailure rabbitMq = AMQP.publishMsg rabbitMq Email.failureExchange


--------------------------------------------------------------------------------
loadTemplates :: Monad m => IO (Heist.HeistState m)
loadTemplates = fmap (either (error . show) id) $ do
  templatesDir <- Email.getDataFileName "templates"
  Error.runEitherT $ do
    templateRepo <- Heist.loadTemplates templatesDir
    Heist.initHeist mempty { Heist.hcTemplates = templateRepo }
