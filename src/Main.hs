{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

import Control.Monad (forever, void)
import Data.Data

import qualified Data.Aeson.Generic as Aeson
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail

deriving instance Data Mail.Address
deriving instance Data Mail.Encoding
deriving instance Data Mail.Mail
deriving instance Data Mail.Part

deriving instance Typeable Mail.Address
deriving instance Typeable Mail.Encoding
deriving instance Typeable Mail.Mail
deriving instance Typeable Mail.Part

sendEmail :: AMQP.Message -> AMQP.Envelope -> IO ()
sendEmail msg env = do
  case Aeson.decode (AMQP.msgBody msg) of
    Just mail -> Mail.renderSendMail mail
    Nothing -> putStrLn "FAILED TO SEND AN EMAIL! OH MY GOD"

  AMQP.ackEnv env

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

  AMQP.consumeMsgs rabbitMq outboxQueue AMQP.Ack (uncurry sendEmail)

  void $ forever getLine
 where
  outboxExchange = "outbox"
