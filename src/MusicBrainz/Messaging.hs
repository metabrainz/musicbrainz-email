{-# LANGUAGE RecordWildCards #-}
module MusicBrainz.Messaging
    ( RabbitMQConnection(..)
    , connect
    ) where

--------------------------------------------------------------------------------
import qualified Network.AMQP as AMQP


--------------------------------------------------------------------------------
data RabbitMQConnection = RabbitMQConnection { rabbitHost :: String
                                             , rabbitVHost :: String
                                             , rabbitUser :: String
                                             , rabbitPassword :: String
                                             }


--------------------------------------------------------------------------------
connect :: RabbitMQConnection -> IO AMQP.Connection
connect RabbitMQConnection {..} =
  AMQP.openConnection rabbitHost rabbitVHost rabbitUser rabbitPassword
