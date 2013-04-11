{-# LANGUAGE OverloadedStrings #-}
module Retry (Options(..), RetryCommand(..), retry) where

--------------------------------------------------------------------------------
import Control.Monad ((>=>), forM_, void)
import Data.List (nub)
import Data.Traversable (traverse)


--------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Network.AMQP as AMQP

import Data.Aeson ((.:?))

--------------------------------------------------------------------------------
import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging


--------------------------------------------------------------------------------
data RetryCommand = Unroutable | Invalid
  deriving (Eq)


--------------------------------------------------------------------------------
data Options = Options Messaging.RabbitMQConnection [RetryCommand]


--------------------------------------------------------------------------------
retry :: Options -> IO ()
retry (Options rabbitConf commands) = do
    rabbitMqConn <- Messaging.connect rabbitConf

    recvChan <- AMQP.openChannel rabbitMqConn
    sendChan <- AMQP.openChannel rabbitMqConn

    Email.establishRabbitMqConfiguration recvChan

    forM_ (nub commands) $ \command ->
      consume recvChan (commandQueue command) $
        \(msg, _) -> consumeMessage command sendChan msg

 where

  consumeMessage Invalid sendChan msg =
    AMQP.publishMsg sendChan Email.outboxExchange "" msg

  consumeMessage Unroutable sendChan msg = void $
    maybe
      (AMQP.publishMsg sendChan Email.failureExchange Email.invalidKey msg)
      (void . traverse (Email.enqueueEmail sendChan))
      (Aeson.decode (AMQP.msgBody msg) >>= parseEmail)

   where

    parseEmail (Aeson.Object o) = Aeson.parseMaybe (.:? "email") o
    parseEmail _                = Nothing

  commandQueue Invalid = Email.invalidQueue
  commandQueue Unroutable = Email.unroutableQueue

  consume chan queue callback =
    let go = maybe (return ()) (callback >=> const go) =<<
               AMQP.getMsg chan AMQP.NoAck queue
    in go
