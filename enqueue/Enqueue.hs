{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Enqueue (Command(..), Options(..), RetryCommand(..), run) where

--------------------------------------------------------------------------------
import Control.Monad ((>=>), forM_, when, void)
import Control.Monad.Trans (lift)
import Data.List (nub)
import Data.Traversable (traverse)


--------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail

import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import Data.Aeson ((.:?))
import Database.PostgreSQL.Simple.SqlQQ (sql)


--------------------------------------------------------------------------------
import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging


--------------------------------------------------------------------------------
data Command = PasswordReset PG.ConnectInfo
             | Retry [RetryCommand]


--------------------------------------------------------------------------------
data RetryCommand = Unroutable | Invalid
  deriving (Eq)


--------------------------------------------------------------------------------
data Options = Options Command Messaging.RabbitMQConnection


--------------------------------------------------------------------------------
evaluateCommand :: Command -> ReaderT AMQP.Connection IO [Email.Email]


--------------------------------------------------------------------------------
evaluateCommand (PasswordReset connInfo) = lift $ do
  pg <- PG.connect connInfo

  PG.fold_ pg editorsWithEmailAddresses [] go

 where
  go emails (editorName, emailAddress) =
    let email = Email.Email
          { Email.emailTemplate = Email.PasswordReset editorName
          , Email.emailTo = Mail.Address
              { Mail.addressName = Just editorName
              , Mail.addressEmail = emailAddress
              }
          , Email.emailFrom = Mail.Address
              { Mail.addressName = Just "MusicBrainz"
              , Mail.addressEmail = "noreply@musicbrainz.org"
              }
          }
    in return (email : emails)

  editorsWithEmailAddresses =
    [sql| SELECT name, email
          FROM editor
          WHERE email IS DISTINCT FROM ''
            AND email_confirm_date IS NOT NULL
            AND last_login_date < '2013-03-29'
    |]


--------------------------------------------------------------------------------
evaluateCommand (Retry commands) = do
  rabbitMqConn <- ask

  lift $ do
    recvChan <- AMQP.openChannel rabbitMqConn
    sendChan <- AMQP.openChannel rabbitMqConn

    Email.establishRabbitMqConfiguration recvChan

    forM_ (nub commands) $ \command ->
      consume recvChan (commandQueue command) $
        \(msg, _) -> consumeMessage command sendChan msg

  return []

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


--------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options command r) = do
  rabbitMqConn <- Messaging.connect r

  emails <- runReaderT (evaluateCommand command) rabbitMqConn
  when (not . null $ emails) $ do
    rabbitMq <- AMQP.openChannel rabbitMqConn
    mapM_ (Email.enqueueEmail rabbitMq) emails
