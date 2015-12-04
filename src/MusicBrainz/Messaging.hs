{-# LANGUAGE RecordWildCards #-}
module MusicBrainz.Messaging
    ( RabbitMQConnection(..)
    , connect
    , rabbitOptparse
    ) where

--------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$>))
import Data.Monoid (mconcat)
import Data.Text (Text)

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Network.AMQP as AMQP
import qualified Options.Applicative as Optparse


--------------------------------------------------------------------------------
data RabbitMQConnection = RabbitMQConnection { rabbitHost :: String
                                             , rabbitVHost :: Text
                                             , rabbitUser :: Text
                                             , rabbitPassword :: Text
                                             }


--------------------------------------------------------------------------------
connect :: RabbitMQConnection -> IO AMQP.Connection
connect RabbitMQConnection {..} =
  AMQP.openConnection rabbitHost rabbitVHost rabbitUser rabbitPassword


--------------------------------------------------------------------------------
parseTextOption :: Optparse.Mod Optparse.OptionFields String -> Optparse.Parser Text
parseTextOption = \x -> T.pack <$> Optparse.strOption x

rabbitOptparse :: Optparse.Parser RabbitMQConnection
rabbitOptparse =
  RabbitMQConnection
    <$> (Optparse.strOption (
                          mconcat [ Optparse.long "rabbit-host"
                                  , Optparse.value "127.0.0.1"
                                  , Optparse.help "RabbitMQ host"
                                  ])
                            )
    <*> (parseTextOption (mconcat [ Optparse.long "rabbit-vhost"
                                  , Optparse.value "/email"
                                  , Optparse.help "RabbitMQ virtual host for email"
                                  ]))
    <*> (parseTextOption (mconcat [ Optparse.long "rabbit-user"
                                  , Optparse.value "guest"
                                  , Optparse.help "RabbitMQ username"
                                  ]))
    <*> (parseTextOption (mconcat [ Optparse.long "rabbit-password"
                                  , Optparse.value "guest"
                                  , Optparse.help "RabbitMQ username"
                                  ]))
