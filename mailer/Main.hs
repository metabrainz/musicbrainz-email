{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad (forever, void)


--------------------------------------------------------------------------------
import qualified Network.Mail.Mime as Mail
import qualified Network.AMQP as AMQP


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

  sendMail <- let approximateEditorCount = 680000
                  day = 24 * 60 * 60.0
              in RateLimit.rateLimit (approximateEditorCount / day)
                   Mail.renderSendMail

  heist <- Mailer.loadTemplates
  Mailer.consumeOutbox rabbitMqConn heist sendMail

  void $ forever getLine
