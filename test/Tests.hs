{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Chan as Chan
import Data.Monoid (mempty)
import GHC.Generics (Generic)

import Database.PostgreSQL.Simple.SqlQQ (sql)


--------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Generic as GAeson
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

import Data.Aeson ((.=))
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
                         , invalidMessageRouting
                         , sendMailFailureRouting
                         ]


--------------------------------------------------------------------------------
enqueuePasswordResets :: Tests.Test
enqueuePasswordResets = withTimeOut $
  Tests.testCase "Will send password reset emails to editors" $
    withRabbitMq $ \(rabbitMq, _) -> do
      pg <- PG.connect testPg

      insertTestData pg

      sentMessages <- spyQueue rabbitMq Email.outboxQueue

      liftIO (Enqueue.run (Enqueue.Options (Enqueue.GoPasswordReset testPg) testRabbitSettings))

      sentMessage <- Chan.readChan sentMessages
      (GAeson.decode (AMQP.msgBody sentMessage)) @?= Just expected

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
    withRabbitMq $ \(rabbitMq, rabbitMqConn) -> do
      heist <- Mailer.loadTemplates

      sentEmails <- Chan.newChan
      Mailer.consumeOutbox rabbitMqConn (Chan.writeChan sentEmails)

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = GAeson.encode testEmail }

      sentEmail <- Chan.readChan sentEmails
      Just sentEmail @?= Mailer.emailToMail testEmail heist


--------------------------------------------------------------------------------
testEmail :: Email.Email
testEmail = Email.Email
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
invalidMessageRouting :: Tests.Test
invalidMessageRouting = withTimeOut $
  Tests.testCase "Unparsable emails are forwarded to outbox.invalid" $ do
    withRabbitMq $ \(rabbitMq, rabbitMqConn) -> do
      invalidMessages <- spyQueue rabbitMq Email.invalidQueue

      Mailer.consumeOutbox rabbitMqConn (const $ return ())

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = invalidRequest }

      invalidMessage <- Chan.readChan invalidMessages
      AMQP.msgBody invalidMessage @?= invalidRequest

 where

  invalidRequest = LBS.fromChunks [Encoding.encodeUtf8 "Ceci n'est pas une JSON-request"]


--------------------------------------------------------------------------------
sendMailFailureRouting :: Tests.Test
sendMailFailureRouting = withTimeOut $
  Tests.testCase "If sendmail doesn't exit cleanly, messages are forwarded to outbox.unroutable" $ do
    withRabbitMq $ \(rabbitMq, rabbitMqConn) -> do
      unroutableMessages <- spyQueue rabbitMq Email.unroutableQueue

      Mailer.consumeOutbox rabbitMqConn (const $ error errorMessage)

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = GAeson.encode testEmail }

      unroutableMessage <- Chan.readChan unroutableMessages
      Aeson.decode (AMQP.msgBody unroutableMessage)
        @?= Just (Aeson.object [ "error" .= errorMessage
                               , "email" .= GAeson.encode testEmail
                               ])

 where

  errorMessage = "Kaboom!"


--------------------------------------------------------------------------------
withRabbitMq :: ((AMQP.Channel, AMQP.Connection) -> IO a) -> IO a
withRabbitMq = bracket acquire release
 where

  acquire = do
    rabbitMqConn <- Messaging.connect testRabbitSettings
    rabbitMq <- AMQP.openChannel rabbitMqConn
    Email.establishRabbitMqConfiguration rabbitMq

    AMQP.purgeQueue rabbitMq Email.outboxQueue

    return (rabbitMq, rabbitMqConn)

  release (_, conn) = AMQP.closeConnection conn


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
  Tests.plusTestOptions mempty { Tests.topt_timeout = Just (Just 50000000) }


--------------------------------------------------------------------------------
spyQueue :: AMQP.Channel
         -> String
         -> IO (Chan.Chan AMQP.Message)
spyQueue rabbitMq queue = do
  sentMessages <- Chan.newChan

  AMQP.consumeMsgs rabbitMq queue AMQP.NoAck $ \(message, _) ->
    Chan.writeChan sentMessages message

  return sentMessages
