{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding (catch)


--------------------------------------------------------------------------------
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.Data


--------------------------------------------------------------------------------
import qualified Data.Aeson.Generic as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


--------------------------------------------------------------------------------
deriving instance Data Mail.Address
deriving instance Data Mail.Encoding
deriving instance Data Mail.Mail
deriving instance Data Mail.Part

deriving instance Typeable Mail.Address
deriving instance Typeable Mail.Encoding
deriving instance Typeable Mail.Mail
deriving instance Typeable Mail.Part

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

  return $ \msg env -> do
    case Aeson.decode (AMQP.msgBody msg) of
      Just mail ->
        Mail.renderSendMail mail `catch`
          (\e -> let errorString = Text.pack $ show (e :: SomeException)
                 in AMQP.publishMsg rabbitMq failureExchange unroutableKey
                      AMQP.newMsg
                        { AMQP.msgBody =
                            LBS.fromChunks [ Text.encodeUtf8 errorString ]
                        })
      Nothing ->
        AMQP.publishMsg rabbitMq failureExchange invalidKey msg

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
