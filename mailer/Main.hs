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

--------------------------------------------------------------------------------
main :: IO ()
main = do
  rabbitMqConn <- AMQP.openConnection "127.0.0.1" "/email" "guest" "guest"
  rabbitMq <- AMQP.openChannel rabbitMqConn

  Email.establishRabbitMqConfiguration rabbitMq
  Mailer.consumeOutbox rabbitMqConn Mail.renderSendMail

  void $ forever getLine
