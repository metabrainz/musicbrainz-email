{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Chan as Chan
import Data.Monoid (mempty)
import GHC.Generics (Generic)

import Database.PostgreSQL.Simple.SqlQQ (sql)


--------------------------------------------------------------------------------
import qualified Data.Aeson.Generic as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Test.SmallCheck.Series as SmallCheck
import qualified Test.Framework as Tests
import qualified Test.Framework.Providers.HUnit as Tests
import qualified Test.Framework.Providers.SmallCheck as Tests

import Test.HUnit ((@?=))

--------------------------------------------------------------------------------
import qualified Enqueue
import qualified Mailer

import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging

--------------------------------------------------------------------------------
main :: IO ()
main = Tests.defaultMain [ enqueuePasswordResets
                         , expandTemplates
                         , messagesAreSent
                         ]


--------------------------------------------------------------------------------
enqueuePasswordResets :: Tests.Test
enqueuePasswordResets = withTimeOut $
  Tests.testCase "Will send password reset emails to editors" $ do
    (rabbitMq, _) <- testRabbit
    pg <- PG.connect testPg

    insertTestData pg

    sentMessages <- spyOutbox rabbitMq

    liftIO (Enqueue.run (Enqueue.Options (Enqueue.GoPasswordReset testPg) testRabbitSettings))

    sentMessage <- Chan.readChan sentMessages
    sentMessage @?= Just expected

 where

  expected = Email.Email
    { Email.emailTo = Mail.Address { Mail.addressEmail = "ollie@ocharles.org.uk"
                                   , Mail.addressName = Just "ocharles"
                                   }
    , Email.emailFrom = Mail.Address { Mail.addressEmail = "noreply@musicbrainz.org"
                                     , Mail.addressName = Just "MusicBrainz"
                                     }
    , Email.emailTemplate =
        Email.PasswordReset { Email.passwordResetEditor = "ocharles" }
    }

  testPg = PG.ConnectInfo { PG.connectUser = "musicbrainz"
                          , PG.connectPassword = ""
                          , PG.connectPort = 5432
                          , PG.connectDatabase = "musicbrainz_test"
                          , PG.connectHost = "localhost"
                          }

  spyOutbox rabbitMq = do
    sentMessages <- Chan.newChan

    (spy, _, _) <- AMQP.declareQueue rabbitMq
                     AMQP.newQueue { AMQP.queueExclusive = True
                                   , AMQP.queueAutoDelete = True }

    AMQP.bindQueue rabbitMq spy Email.outboxExchange ""
    AMQP.consumeMsgs rabbitMq spy AMQP.Ack $ \(message, _) ->
      Chan.writeChan sentMessages (Aeson.decode $ AMQP.msgBody message)

    return sentMessages

  insertTestData pg = do
    PG.execute_ pg "TRUNCATE editor CASCADE"
    PG.execute pg
      [sql| INSERT INTO editor (name, password, email, email_confirm_date)
            VALUES (?, 'ignored', ?, '2010-01-01') |]
      ( Email.passwordResetEditor $ Email.emailTemplate expected
      , Mail.addressEmail $ Email.emailTo expected
      )


--------------------------------------------------------------------------------
instance Monad m => SmallCheck.Serial m Text.Text where
  series = SmallCheck.cons1 Text.pack

deriving instance Generic Mail.Address
instance Monad m => SmallCheck.Serial m Mail.Address

expandTemplates :: Tests.Test
expandTemplates = Tests.buildTest $ do
  heist <- Mailer.loadTemplates
  return $ Tests.testGroup "Can expand templates into real emails"
    [ Tests.withDepth 4 $ Tests.testProperty "Password reset emails" $
        \editor emailTo emailFrom ->
           let Just mail = Mailer.emailToMail
                 Email.Email { Email.emailTemplate = Email.PasswordReset editor
                             , Email.emailTo =
                                 Mail.Address { Mail.addressEmail = emailTo
                                              , Mail.addressName = Just editor
                                              }
                             , Email.emailFrom = emailFrom
                             }
                 heist
               emailBody = Encoding.decodeUtf8 . BS.concat . LBS.toChunks .
                 Mail.partContent . head . head . Mail.mailParts $ mail
           in and $ map (flip Text.isInfixOf emailBody)
                [ changePasswordUrl editor
                , greeting editor
                ]
    ]

 where

  changePasswordUrl =
    Text.append "https://musicbrainz.org/account/change-password?mandatory=1&username="

  greeting = Text.append "Dear "


--------------------------------------------------------------------------------
deriving instance Eq Mail.Encoding
deriving instance Show Mail.Encoding

deriving instance Eq Mail.Mail
deriving instance Show Mail.Mail

deriving instance Eq Mail.Part
deriving instance Show Mail.Part

messagesAreSent :: Tests.Test
messagesAreSent = withTimeOut $
  Tests.testCase "Emails in outbox are sent by outbox consumer" $ do
    (rabbitMq, rabbitMqConn) <- testRabbit

    heist <- Mailer.loadTemplates

    sentEmails <- Chan.newChan
    consumer <- Mailer.emailConsumer rabbitMqConn (Chan.writeChan sentEmails)

    AMQP.consumeMsgs rabbitMq Email.outboxQueue AMQP.Ack (uncurry consumer)

    AMQP.publishMsg rabbitMq Email.outboxExchange ""
      AMQP.newMsg { AMQP.msgBody = Aeson.encode validEmail }

    sentEmail <- Chan.readChan sentEmails
    Just sentEmail @?= Mailer.emailToMail validEmail heist

 where
  validEmail = Email.Email
    { Email.emailTemplate = Email.PasswordReset "ocharles"
    , Email.emailTo =
        Mail.Address { Mail.addressName = Nothing
                     , Mail.addressEmail = "foo@example.com"
                     }
    , Email.emailFrom =
        Mail.Address { Mail.addressName = Just "MusicBrainz"
                     , Mail.addressEmail = "noreply@musicbrainz.org"
                     }
    }


--------------------------------------------------------------------------------
testRabbit :: IO (AMQP.Channel, AMQP.Connection)
testRabbit = do
  rabbitMqConn <- Messaging.connect testRabbitSettings
  rabbitMq <- AMQP.openChannel rabbitMqConn
  Email.establishRabbitMqConfiguration rabbitMq

  AMQP.purgeQueue rabbitMq Email.outboxQueue

  return (rabbitMq, rabbitMqConn)

 where


--------------------------------------------------------------------------------
testRabbitSettings :: Messaging.RabbitMQConnection
testRabbitSettings =
  Messaging.RabbitMQConnection
    { Messaging.rabbitHost = "127.0.0.1"
    , Messaging.rabbitVHost = "/test/email"
    , Messaging.rabbitUser = "guest"
    , Messaging.rabbitPassword = "guest"
    }


--------------------------------------------------------------------------------
withTimeOut :: Tests.Test -> Tests.Test
withTimeOut =
  Tests.plusTestOptions mempty { Tests.topt_timeout = Just (Just 5000000) }
