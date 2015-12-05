{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

--------------------------------------------------------------------------------
#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
#endif

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM.TChan as TChan
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO.Error (catchIOError, isDoesNotExistError)

import Database.PostgreSQL.Simple.SqlQQ (sql)


--------------------------------------------------------------------------------
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator as Configurator
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PG
import qualified Heist
import qualified Network.AMQP as AMQP
import qualified Network.HTTP as HTTP
import qualified Network.Mail.Mime as Mail
import qualified Test.SmallCheck.Series as SmallCheck
import qualified Test.Framework as Tests
import qualified Test.Framework.Providers.HUnit as Tests
import qualified Test.Framework.Providers.SmallCheck as Tests

import Data.Aeson ((.=))
import Test.HUnit ((@?=), (@?))

--------------------------------------------------------------------------------
import qualified Enqueue
import qualified Mailer
import qualified RateLimit

import qualified MusicBrainz.Email as Email
import qualified MusicBrainz.Messaging as Messaging

--------------------------------------------------------------------------------
main :: IO ()
main = do
  testConfig <- Configurator.load [ Configurator.Required "test.cfg" ]
                 `catchIOError`
                  (\e -> if isDoesNotExistError e
                           then error "test.cfg not found. Please add a test.cfg file, see test.cfg.example for more information."
                           else ioError e)

  let opt key def = Configurator.lookupDefault (def PG.defaultConnectInfo) testConfig key
  pgConf <- PG.ConnectInfo
    <$> opt "db.host" PG.connectHost
    <*> opt "db.port" PG.connectPort
    <*> opt "db.user" PG.connectUser
    <*> opt "db.password" PG.connectPassword
    <*> opt "db.database" PG.connectDatabase

  rabbitMqConf <- Messaging.RabbitMQConnection
    <$> Configurator.require testConfig "rabbitmq.host"
    <*> Configurator.require testConfig "rabbitmq.vhost"
    <*> Configurator.require testConfig "rabbitmq.user"
    <*> Configurator.require testConfig "rabbitmq.password"

  Tests.defaultMain $ map withTimeOut $
    [ enqueuePasswordResets pgConf rabbitMqConf
    , expandTemplates
    , messagesAreSent rabbitMqConf
    , invalidMessageRouting rabbitMqConf
    , sendMailFailureRouting rabbitMqConf
    , heistFailureRouting rabbitMqConf
    , rateLimitTests
    ]


--------------------------------------------------------------------------------
enqueuePasswordResets :: PG.ConnectInfo -> Messaging.RabbitMQConnection -> Tests.Test
enqueuePasswordResets pgConf rabbitConf = withTimeOut $
  Tests.testGroup "Enqueueing password reset emails"
    [ Tests.testCase "Will send password reset emails to editors with old login date and confirmed email address" $
        withRabbitMq rabbitConf $ \(rabbitMq, _) -> do
          pg <- emptyPg

          PG.execute pg
            [sql| INSERT INTO editor (name, password, email, email_confirm_date, last_login_date)
                  VALUES (?, 'ignored', ?, '2010-01-01', '2010-01-01') |]
            ( Email.passwordResetEditor $ Email.emailTemplate expected
            , Mail.addressEmail $ Email.emailTo expected
            )

          sentMessages <- spyQueue rabbitMq (Text.unpack Email.outboxQueue)

          liftIO (Enqueue.run (Enqueue.Options (Enqueue.PasswordReset pgConf) rabbitConf))

          sentMessage <- STM.atomically $ TChan.readTChan sentMessages
          (Aeson.decode (AMQP.msgBody sentMessage)) @?= Just expected

    , Tests.testCase "Will not send to editors with unconfirmed email address" $
        withRabbitMq rabbitConf $ \(rabbitMq, _) -> do
          pg <- emptyPg

          PG.execute pg
            [sql| INSERT INTO editor (name, password, email, email_confirm_date, last_login_date)
                  VALUES (?, 'ignored', ?, null, '2010-01-01') |]
            ( Email.passwordResetEditor $ Email.emailTemplate expected
            , Mail.addressEmail $ Email.emailTo expected
            )

          expectNoSentMessages rabbitMq

    , Tests.testCase "Will not send to editors who logged in recently" $
        withRabbitMq rabbitConf $ \(rabbitMq, _) -> do
          pg <- emptyPg

          PG.execute pg
            [sql| INSERT INTO editor (name, password, email, email_confirm_date, last_login_date)
                  VALUES (?, 'ignored', ?, now(), '2013-04-29') |]
            ( Email.passwordResetEditor $ Email.emailTemplate expected
            , Mail.addressEmail $ Email.emailTo expected
            )

          expectNoSentMessages rabbitMq
    ]

 where

  expectNoSentMessages rabbitMq = do
    sentMessages <- spyQueue rabbitMq (Text.unpack Email.outboxQueue)

    liftIO (Enqueue.run (Enqueue.Options (Enqueue.PasswordReset pgConf) rabbitConf))

    (STM.atomically $ TChan.isEmptyTChan sentMessages)
      @? "No emails should have been sent"

  emptyPg = do
    pg <- PG.connect pgConf
    PG.execute_ pg "TRUNCATE editor CASCADE"
    return pg

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


--------------------------------------------------------------------------------
instance Monad m => SmallCheck.Serial m Text.Text where
  series = SmallCheck.cons1 Text.pack

instance Monad m => SmallCheck.Serial m Mail.Address

expandTemplates :: Tests.Test
expandTemplates = Tests.buildTest $ do
  heist <- Mailer.loadTemplates
  return $ Tests.testGroup "Can expand templates into real emails"
    [ Tests.withDepth 4 $ Tests.testProperty "Password reset emails" $
        \editor emailAddress emailFrom ->
           let emailTo = Mail.Address { Mail.addressEmail = emailAddress
                                      , Mail.addressName = Just editor
                                      }
               Just mail = Mailer.emailToMail
                 Email.Email { Email.emailTemplate = Email.PasswordReset editor
                             , Email.emailTo = emailTo
                             , Email.emailFrom = emailFrom
                             }
                 heist
               emailBody = Encoding.decodeUtf8 . BS.concat . LBS.toChunks .
                 Mail.partContent . head . head . Mail.mailParts $ mail
           in and $ [ Mail.mailTo mail == [ emailTo ]
                    , Mail.mailFrom mail == emailFrom
                    ] ++
                    map (flip Text.isInfixOf emailBody)
                      [ changePasswordUrl editor
                      , greeting editor
                      , "https://musicbrainz.org/lost-password"
                      ]
    ]

 where

  changePasswordUrl =
    Text.append "https://musicbrainz.org/account/change-password?mandatory=1&username=" .
      Text.pack . HTTP.urlEncode . Text.unpack

  greeting = Text.append "Dear "


--------------------------------------------------------------------------------
deriving instance Eq Mail.Mail

messagesAreSent :: Messaging.RabbitMQConnection -> Tests.Test
messagesAreSent rabbitConf = withTimeOut $
  Tests.testCase "Emails in outbox are sent by outbox consumer" $ do
    withRabbitMq rabbitConf $ \(rabbitMq, rabbitMqConn) -> do
      heist <- Mailer.loadTemplates

      sentEmails <- STM.atomically $ TChan.newTChan
      Mailer.consumeOutbox rabbitMqConn heist $
        STM.atomically . TChan.writeTChan sentEmails

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = Aeson.encode testEmail }

      sentEmail <- STM.atomically $ TChan.readTChan sentEmails
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
invalidMessageRouting :: Messaging.RabbitMQConnection -> Tests.Test
invalidMessageRouting rabbitConf = withTimeOut $
  Tests.testCase "Unparsable emails are forwarded to outbox.invalid" $ do
    withRabbitMq rabbitConf $ \(rabbitMq, rabbitMqConn) -> do
      invalidMessages <- spyQueue rabbitMq (Text.unpack Email.invalidQueue)

      heist <- Mailer.loadTemplates
      Mailer.consumeOutbox rabbitMqConn heist (const $ return ())

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = invalidRequest }

      invalidMessage <- STM.atomically $ TChan.readTChan invalidMessages
      Aeson.decode (AMQP.msgBody invalidMessage) @?=
        Just (Aeson.object [ "error" .= ("Could not decode JSON" :: String)
                           , "json" .= decodeUtf8 invalidRequest
                           ])

 where

  invalidRequest = LBS.fromChunks [Encoding.encodeUtf8 "Ceci n'est pas une JSON-request"]


--------------------------------------------------------------------------------
sendMailFailureRouting :: Messaging.RabbitMQConnection -> Tests.Test
sendMailFailureRouting rabbitConf = withTimeOut $
  Tests.testCase "If sendmail doesn't exit cleanly, messages are forwarded to outbox.unroutable" $ do
    withRabbitMq rabbitConf $ \(rabbitMq, rabbitMqConn) -> do
      unroutableMessages <- spyQueue rabbitMq (Text.unpack Email.unroutableQueue)

      heist <- Mailer.loadTemplates
      Mailer.consumeOutbox rabbitMqConn heist (const $ error errorMessage)

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = Aeson.encode testEmail }

      unroutableMessage <- STM.atomically $ TChan.readTChan unroutableMessages
      Aeson.decode (AMQP.msgBody unroutableMessage)
        @?= Just (Aeson.object [ "error" .= errorMessage
                               , "email" .= testEmail
                               ])

 where

  errorMessage = "Kaboom!"


--------------------------------------------------------------------------------
heistFailureRouting :: Messaging.RabbitMQConnection -> Tests.Test
heistFailureRouting rabbitConf = withTimeOut $
  Tests.testCase "If Heist can't expand the template, messages are forwarded to outbox.unroutable" $ do
    withRabbitMq rabbitConf $ \(rabbitMq, rabbitMqConn) -> do
      unroutableMessages <- spyQueue rabbitMq (Text.unpack Email.unroutableQueue)

      -- A 'Heist' that doesn't know about any of the templates
      Right emptyHeist <- Either.runEitherT (Heist.initHeist Heist.emptyHeistConfig)
      Mailer.consumeOutbox rabbitMqConn emptyHeist (const $ return ())

      AMQP.publishMsg rabbitMq Email.outboxExchange ""
        AMQP.newMsg { AMQP.msgBody = Aeson.encode testEmail }

      unroutableMessage <- STM.atomically $ TChan.readTChan unroutableMessages
      Aeson.decode (AMQP.msgBody unroutableMessage)
        @?= Just (Aeson.object
                    [ "email" .= testEmail
                    , "error" .= ("Couldn't render template" :: String)
                    ])


--------------------------------------------------------------------------------
withRabbitMq :: Messaging.RabbitMQConnection -> ((AMQP.Channel, AMQP.Connection) -> IO a) -> IO a
withRabbitMq rabbitConf = bracket acquire release
 where

  acquire = do
    rabbitMqConn <- Messaging.connect rabbitConf
    rabbitMq <- AMQP.openChannel rabbitMqConn
    Email.establishRabbitMqConfiguration rabbitMq

    AMQP.purgeQueue rabbitMq Email.outboxQueue

    return (rabbitMq, rabbitMqConn)

  release (_, conn) = AMQP.closeConnection conn


--------------------------------------------------------------------------------
withTimeOut :: Tests.Test -> Tests.Test
withTimeOut =
  Tests.plusTestOptions mempty { Tests.topt_timeout = Just (Just 50000000) }


--------------------------------------------------------------------------------
spyQueue :: AMQP.Channel
         -> String
         -> IO (TChan.TChan AMQP.Message)
spyQueue rabbitMq queue = do
  sentMessages <- STM.atomically $ TChan.newTChan

  AMQP.consumeMsgs rabbitMq (Text.pack queue) AMQP.NoAck $ \(message, _) ->
    STM.atomically $ TChan.writeTChan sentMessages message

  return sentMessages


--------------------------------------------------------------------------------
rateLimitTests :: Tests.Test
rateLimitTests = Tests.testCase "Fast requests are rate limited" $ do
  let limit = 50
      requests = 10
      expected = (fromIntegral requests) / limit

  limitedFunction <- RateLimit.rateLimit limit (const $ return ())

  startTime <- Time.getCurrentTime

  replicateM requests (limitedFunction ())

  duration <- (`Time.diffUTCTime` startTime) <$> Time.getCurrentTime
  duration >= expected @?
    (show requests ++ " requests should take at least 1/5 second, took " ++
     show duration ++ " expected " ++ show expected)
