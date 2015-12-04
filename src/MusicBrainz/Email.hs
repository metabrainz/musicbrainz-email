{-# LANGUAGE DeriveGeneric #-}
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
    , invalidQueue
    , unroutableQueue

      -- * Queueing emails
    , enqueueEmail
    ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import GHC.Generics (Generic)


--------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail


--------------------------------------------------------------------------------
deriving instance Generic Mail.Address

data Email = Email
    { emailTemplate :: Template
    , emailTo :: Mail.Address
    , emailFrom :: Mail.Address
    }
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Mail.Address
instance Aeson.FromJSON Email

instance Aeson.ToJSON Mail.Address
instance Aeson.ToJSON Email


--------------------------------------------------------------------------------
data Template = PasswordReset { passwordResetEditor :: Text }
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Template

instance Aeson.ToJSON Template

--------------------------------------------------------------------------------
outboxExchange, failureExchange :: Text
invalidKey, unroutableKey :: Text
outboxQueue, unroutableQueue, invalidQueue :: Text

outboxExchange = T.pack "outbox"
failureExchange = T.pack "failure"

invalidKey = T.pack "invalid"
unroutableKey = T.pack "unroutable"

outboxQueue = T.pack "outbox"
invalidQueue = T.pack "outbox.invalid"
unroutableQueue = T.pack "outbox.unroutable"

--------------------------------------------------------------------------------
establishRabbitMqConfiguration :: AMQP.Channel -> IO ()
establishRabbitMqConfiguration rabbitMq = do
  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = outboxExchange
                     , AMQP.exchangeType = T.pack "fanout"
                     }

  AMQP.declareExchange rabbitMq
    AMQP.newExchange { AMQP.exchangeName = failureExchange
                     , AMQP.exchangeType = T.pack "direct"
                     }


  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = outboxQueue }

  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = invalidQueue }

  AMQP.declareQueue rabbitMq
    AMQP.newQueue { AMQP.queueName = unroutableQueue }


  AMQP.bindQueue rabbitMq outboxQueue outboxExchange (T.pack "")
  AMQP.bindQueue rabbitMq invalidQueue failureExchange invalidKey
  AMQP.bindQueue rabbitMq unroutableQueue failureExchange unroutableKey


--------------------------------------------------------------------------------
enqueueEmail :: AMQP.Channel -> Email -> IO ()
enqueueEmail rabbitMq email =
  AMQP.publishMsg rabbitMq outboxExchange (T.pack "")
    AMQP.newMsg
      { AMQP.msgBody = Aeson.encode email
      , AMQP.msgDeliveryMode = Just AMQP.Persistent
      }
