{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Enqueue where

--------------------------------------------------------------------------------
import qualified Data.Aeson.Generic as Aeson
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail

import Database.PostgreSQL.Simple.SqlQQ (sql)

--------------------------------------------------------------------------------
import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging


--------------------------------------------------------------------------------
data Command = GoPasswordReset PG.ConnectInfo


--------------------------------------------------------------------------------
data Options = Options Command Messaging.RabbitMQConnection


--------------------------------------------------------------------------------
evaluateCommand :: Command -> IO [Email.Email]
evaluateCommand (GoPasswordReset connInfo) = do
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
            AND last_login_date < '2013-04-29'
    |]

--------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options command r) = do
  rabbitMq <- Messaging.connect r >>= AMQP.openChannel
  evaluateCommand command >>= mapM_ (enqueueEmail rabbitMq)

 where

  enqueueEmail rabbitMq email =
    AMQP.publishMsg rabbitMq Email.outboxExchange ""
      AMQP.newMsg
        { AMQP.msgBody = Aeson.encode email
        , AMQP.msgDeliveryMode = Just AMQP.Persistent
        }
