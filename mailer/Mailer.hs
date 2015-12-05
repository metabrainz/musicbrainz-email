{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailer where

--------------------------------------------------------------------------------
import Prelude


--------------------------------------------------------------------------------
#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Exception (SomeException, try)
import Control.Lens (set)
import Control.Monad (void)
import Data.Aeson ((.=))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Syntax ((##))
import Data.Text.Lazy.Encoding (decodeUtf8)


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Builder
import qualified Control.Error.Util as ErrorUtil
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Heist as Heist
import qualified Heist.Interpreted as Heist
import qualified Network.AMQP as AMQP
import qualified Network.HTTP as HTTP
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
    (Heist.bindSplice "urlEncode" urlEncode $
     Heist.bindStrings templateBindings heist)

 where

  urlEncode =
    map (XmlHtml.TextNode . Text.pack . HTTP.urlEncode . Text.unpack . XmlHtml.nodeText)
      <$> Heist.runChildren

  mailBuilder = fmap (makeMail . runTemplate) <$> Heist.evalTemplate templatePath

  runTemplate = Builder.toLazyByteString . XmlHtml.renderHtmlFragment XmlHtml.UTF8

  template = Email.emailTemplate email

  templatePath = case template of
    (Email.PasswordReset _) -> "password-reset"

  templateBindings = case template of
    (Email.PasswordReset editor) -> ("editor" ## editor)

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
        (publishFailure rabbitMq Email.invalidKey (unableToDecode msg))
        (Either.eitherT (publishFailure rabbitMq Email.unroutableKey) return .
           trySendEmail)
        (Aeson.decode $ AMQP.msgBody msg)

      AMQP.ackEnv env

 where

  getMsgBodyText = decodeUtf8 . AMQP.msgBody

  unableToDecode msg =
    AMQP.newMsg { AMQP.msgBody = Aeson.encode $ Aeson.object
                   [ "error" .= ("Could not decode JSON" :: String)
                   , "json" .= getMsgBodyText msg
                   ]
                }

  trySendEmail email = tryFormEmail >>= trySend

   where

    failureMessage e =
      AMQP.newMsg { AMQP.msgBody = Aeson.encode $ Aeson.object
                      [ "email" .= email
                      , "error" .= (e :: Text.Text)
                      ]
                  }

    tryFormEmail =
      Either.EitherT $ return $
        ErrorUtil.note (failureMessage "Couldn't render template") $
          emailToMail email heist

    trySend mail =
      let exceptionMessage e =
            failureMessage (Text.pack (show (e :: SomeException)))
      in Either.bimapEitherT exceptionMessage id $
           Either.EitherT $ try (sendMail mail)

  publishFailure rabbitMq = AMQP.publishMsg rabbitMq Email.failureExchange


--------------------------------------------------------------------------------
loadTemplates :: Monad m => IO (Heist.HeistState m)
loadTemplates = fmap (either (error . show) id) $ do
  templatesDir <- Email.getDataFileName "templates"
  Either.runEitherT $ do
    Heist.initHeist $
      (set Heist.hcNamespace "") $
        (set Heist.hcTemplateLocations [Heist.loadTemplates templatesDir]) $
          Heist.emptyHeistConfig
