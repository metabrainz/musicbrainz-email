{-# LANGUAGE RecordWildCards #-}
module MusicBrainz.Messaging
    ( RabbitMQConnection(..)
    , connect
    , rabbitOptparse
    ) where

--------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$>))
import Data.Monoid (mconcat)


--------------------------------------------------------------------------------
import qualified Network.AMQP as AMQP
import qualified Options.Applicative as Optparse


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


--------------------------------------------------------------------------------
rabbitOptparse :: Optparse.Parser RabbitMQConnection
rabbitOptparse =
  RabbitMQConnection
    <$> Optparse.strOption (mconcat [ Optparse.long "rabbit-host"
                                    , Optparse.value "127.0.0.1"
                                    , Optparse.help "RabbitMQ host"
                                    ])
    <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-vhost"
                                    , Optparse.value "/email"
                                    , Optparse.help "RabbitMQ virtual host for email"
                                    ])
    <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-user"
                                    , Optparse.value "guest"
                                    , Optparse.help "RabbitMQ username"
                                    ])
    <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-password"
                                    , Optparse.value "guest"
                                    , Optparse.help "RabbitMQ username"
                                    ])
