{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad (forever, void)


--------------------------------------------------------------------------------
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Network.Metric as Metrics


--------------------------------------------------------------------------------
import qualified Mailer
import qualified MusicBrainz.Email as Email
import qualified RateLimit

--------------------------------------------------------------------------------
main :: IO ()
main = do
  rabbitMqConn <- AMQP.openConnection "127.0.0.1" "/email" "guest" "guest"
  rabbitMq <- AMQP.openChannel rabbitMqConn

  Email.establishRabbitMqConfiguration rabbitMq

  statsd <- Metrics.open Metrics.Statsd "musicbrainz" "10.1.1.105" 8125
  sendMail <- let approximateEditorCount = 680000
                  day = 24 * 60 * 60.0
              in RateLimit.rateLimit (approximateEditorCount / day) $
                   \mail -> do
                     Mail.renderSendMail mail
                     Metrics.push statsd $
                       Metrics.Counter "email" "sent" 1

  heist <- Mailer.loadTemplates
  Mailer.consumeOutbox rabbitMqConn heist sendMail

  void $ forever getLine
