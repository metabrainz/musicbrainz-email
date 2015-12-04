{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

--------------------------------------------------------------------------------
#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mconcat, mempty)
#endif

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Options.Applicative ((<**>))
import System.IO.Error (catchIOError)


--------------------------------------------------------------------------------
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
data Options = Options Messaging.RabbitMQConnection StatsdConfiguration


--------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options rabbitMqConf Statsd {..}) = do
  rabbitMqConn <- Messaging.connect rabbitMqConf
  rabbitMq <- AMQP.openChannel rabbitMqConn

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
  Mailer.consumeOutbox rabbitMqConn heist sendMail

  forever (threadDelay 1000000)


--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= run

 where

  parser =
    Optparse.info
      (Options <$> Messaging.rabbitOptparse
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
