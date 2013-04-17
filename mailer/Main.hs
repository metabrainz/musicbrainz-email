{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>), (<**>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Monoid (mconcat, mempty)
import System.IO.Error (catchIOError)


--------------------------------------------------------------------------------
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Network.Metric as Metrics
import qualified Options.Applicative as Optparse
import qualified Network.Socket as Socket

--------------------------------------------------------------------------------
import qualified Mailer
import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging
import qualified RateLimit


--------------------------------------------------------------------------------
data StatsdConfiguration = Statsd { statsdHost :: String
                                  , statsdPort :: Socket.PortNumber
                                  }


--------------------------------------------------------------------------------
data Options = Options Messaging.RabbitMQConnection PG.ConnectInfo StatsdConfiguration


--------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options rabbitMqConf db Statsd {..}) = do
  rabbitMqConn <- Messaging.connect rabbitMqConf
  rabbitMq <- AMQP.openChannel rabbitMqConn

  pg <- PG.connect db

  Email.establishRabbitMqConfiguration rabbitMq

  statsd <- Metrics.open Metrics.Statsd "musicbrainz" statsdHost statsdPort
  sendMail <- let approximateEditorCount = 680000
                  day = 24 * 60 * 60.0
              in RateLimit.rateLimit (approximateEditorCount / day) $
                   \mail -> do
                     Mail.renderSendMail mail
                     Metrics.push statsd (Metrics.Counter "email" "sent" 1)
                       `catchIOError` (const $ putStrLn "Couldn't write to statsd")

  heist <- Mailer.loadTemplates
  Mailer.consumeOutbox rabbitMqConn pg heist sendMail

  forever (threadDelay 1000000)


--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= run

 where

  parser =
    Optparse.info
      (Options <$> Messaging.rabbitOptparse
               <*> dbOptions
               <*> statsdParser
               <**> Optparse.helper)
          mempty

  statsdParser =
    Statsd
      <$> Optparse.strOption (mconcat [ Optparse.long "statsd-host"
                                      , Optparse.help "Statsd host"
                                      ])
      <*> fmap fromInteger
            (Optparse.option (mconcat [ Optparse.long "statsd-port"
                                      , Optparse.value 8125
                                      , Optparse.help "Statsd port"
                                      ]))


  dbOptions =
    PG.ConnectInfo
      <$> Optparse.strOption (mconcat [ Optparse.long "db-host"
                                      , Optparse.value "localhost"
                                      , Optparse.help "PostgreSQL database host"
                                      ])
      <*> Optparse.option (mconcat [ Optparse.long "db-port"
                                   , Optparse.value 5432
                                   , Optparse.help "PostgreSQL database port"
                                   ])
      <*> Optparse.strOption (mconcat [ Optparse.long "db-user"
                                      , Optparse.value "musicbrainz"
                                      , Optparse.help "PostgreSQL database username"
                                      ])
      <*> Optparse.strOption (mconcat [ Optparse.long "db-password"
                                      , Optparse.value ""
                                      , Optparse.help "PostgreSQL database password"
                                      ])
      <*> Optparse.strOption (mconcat [ Optparse.long "db"
                                      , Optparse.help "Name of the MusicBrainz database in PostgreSQL"
                                      , Optparse.value "musicbrainz"
                                      ])
