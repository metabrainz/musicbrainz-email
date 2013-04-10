{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad (forever, void)


--------------------------------------------------------------------------------
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

  consumer <- Mailer.emailConsumer rabbitMqConn
  AMQP.consumeMsgs rabbitMq Email.outboxQueue AMQP.Ack (uncurry consumer)

  void $ forever getLine
