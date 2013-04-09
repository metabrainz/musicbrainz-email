{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding (catch)


--------------------------------------------------------------------------------
import Control.Applicative
import Control.Exception (SomeException, catch)
import Control.Monad (forever, join, void)
import Control.Monad.Trans.Either (runEitherT)
import Data.Data
import Data.Functor.Identity (Identity, runIdentity)


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Aeson.Generic as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Heist as Heist
import qualified Heist.Interpreted as Heist
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Text.XmlHtml as XmlHtml


--------------------------------------------------------------------------------
deriving instance Data Mail.Address
deriving instance Typeable Mail.Address

data Email = Email
    { emailTemplate :: Template
    , emailTo :: Mail.Address
    , emailFrom :: Mail.Address
    }
  deriving (Data, Typeable)


--------------------------------------------------------------------------------
data Template = PasswordReset { passwordResetEditor :: Text.Text }
  deriving (Data, Typeable)


--------------------------------------------------------------------------------
emailToMail :: Email -> Heist.HeistState Identity -> Maybe Mail.Mail
emailToMail email heist = runIdentity $
  Heist.evalHeistT
    mailBuilder
    (XmlHtml.TextNode "")
    (Heist.bindStrings templateBindings heist)

 where

  mailBuilder = fmap (makeMail . runTemplate) <$> Heist.evalTemplate templatePath

  runTemplate = Builder.toLazyByteString . XmlHtml.renderHtmlFragment XmlHtml.UTF8

  template = emailTemplate email

  templatePath = case template of
    (PasswordReset _) -> "password-reset"

  templateBindings = case template of
    (PasswordReset editor) -> [("editor", editor)]

  emailSubject = case template of
    (PasswordReset _) -> "Mandatory Password Reset"

  makeMail messageBody = Mail.Mail
    { Mail.mailFrom = emailFrom email
    , Mail.mailTo = [ emailTo email ]
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
  AMQP.bindQueue rabbitMq invalidQueue failureExchange unroutableKey

  heist <- fmap (either (error . show) id) $ runEitherT $ do
      templateRepo <- Heist.loadTemplates "templates"
      Heist.initHeist (Heist.HeistConfig [] [] [] [] templateRepo)

  return $ \msg env -> do
    case Aeson.decode (AMQP.msgBody msg) of
      -- JSON decoding failed
      Nothing ->
        AMQP.publishMsg rabbitMq failureExchange invalidKey msg

      Just email -> do
        case emailToMail email heist of
          -- Template rendering failed
          Nothing ->
            AMQP.publishMsg rabbitMq failureExchange invalidKey msg

          Just mail ->
            Mail.renderSendMail mail `catch`
              (\e -> let errorString = Text.pack $ show (e :: SomeException)
                     in AMQP.publishMsg rabbitMq failureExchange unroutableKey
                          AMQP.newMsg
                            { AMQP.msgBody =
                                LBS.fromChunks [ Text.encodeUtf8 errorString ]
                            })

    AMQP.ackEnv env

 where

  failureExchange = "failure"
  invalidKey = "invalid"
  unroutableKey = "unroutable"


--------------------------------------------------------------------------------
main :: IO ()
main = do
  rabbitMqConn <- AMQP.openConnection "127.0.0.1" "/email" "guest" "guest"
  rabbitMq <- AMQP.openChannel rabbitMqConn

  (outboxQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox" }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = outboxExchange
                     , AMQP.exchangeType = "fanout"
                     }

  AMQP.bindQueue rabbitMq outboxQueue outboxExchange ""

  consumer <- emailConsumer rabbitMqConn
  AMQP.consumeMsgs rabbitMq outboxQueue AMQP.Ack (uncurry consumer)

  void $ forever getLine
 where
  outboxExchange = "outbox"
