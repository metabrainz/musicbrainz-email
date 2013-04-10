{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module MusicBrainz.Email
    ( Email(..)
    , Template(..)

      -- * Messaging Setup
    , establishRabbitMqConfiguration

      -- * Exchanges
    , outboxExchange
    , failureExchange

      -- * Routing keys
    , invalidKey
    , unroutableKey

      -- * Queues
    , outboxQueue
    ) where

--------------------------------------------------------------------------------
import Data.Data (Data, Typeable)


--------------------------------------------------------------------------------
import qualified Data.Text as Text
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail


--------------------------------------------------------------------------------
deriving instance Data Mail.Address
deriving instance Eq Mail.Address
deriving instance Show Mail.Address
deriving instance Typeable Mail.Address

data Email = Email
    { emailTemplate :: Template
    , emailTo :: Mail.Address
    , emailFrom :: Mail.Address
    }
  deriving (Data, Eq, Show, Typeable)


--------------------------------------------------------------------------------
data Template = PasswordReset { passwordResetEditor :: Text.Text }
  deriving (Data, Eq, Show, Typeable)


--------------------------------------------------------------------------------
outboxExchange, failureExchange :: String
invalidKey, unroutableKey :: String
outboxQueue :: String

outboxExchange = "outbox"
failureExchange = "failure"
invalidKey = "invalid"
unroutableKey = "unroutable"
outboxQueue = "outbox"

--------------------------------------------------------------------------------
establishRabbitMqConfiguration :: AMQP.Channel -> IO ()
establishRabbitMqConfiguration rabbitMq = do
  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = outboxExchange
                     , AMQP.exchangeType = "fanout"
                     }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = failureExchange
                     , AMQP.exchangeType = "direct"
                     }


  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = outboxQueue }

  (invalidQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox.invalid" }

  (unroutableQueue, _, _) <- AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = "outbox.unroutable" }


  AMQP.bindQueue rabbitMq outboxQueue outboxExchange ""
  AMQP.bindQueue rabbitMq invalidQueue failureExchange invalidKey
  AMQP.bindQueue rabbitMq unroutableQueue failureExchange unroutableKey
