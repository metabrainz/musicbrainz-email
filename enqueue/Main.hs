{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>), (<**>))
import Data.Monoid (mconcat, mempty)


--------------------------------------------------------------------------------
import qualified Data.Aeson.Generic as Aeson
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.AMQP as AMQP
import qualified Network.Mail.Mime as Mail
import qualified Options.Applicative as Optparse

import Database.PostgreSQL.Simple.SqlQQ (sql)

--------------------------------------------------------------------------------
import MusicBrainz.Email


--------------------------------------------------------------------------------
data Command = GoPasswordReset PG.ConnectInfo


--------------------------------------------------------------------------------
data RabbitMQConnection = RabbitMQConnection { rabbitHost :: String
                                             , rabbitVHost :: String
                                             , rabbitUser :: String
                                             , rabbitPassword :: String
                                             }


--------------------------------------------------------------------------------
data Options = Options Command RabbitMQConnection


--------------------------------------------------------------------------------
evaluateCommand :: Command -> IO [Email]
evaluateCommand (GoPasswordReset connInfo) = do
  pg <- PG.connect connInfo

  PG.fold_ pg editorsWithEmailAddresses [] go

 where
  go emails (editorName, emailAddress) =
    let email = Email
          { emailTemplate = PasswordReset editorName
          , emailTo = Mail.Address
              { Mail.addressName = Just editorName
              , Mail.addressEmail = emailAddress
              }
          , emailFrom = Mail.Address
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
  rabbitMqConn <- AMQP.openConnection
    (rabbitHost r) (rabbitVHost r) (rabbitUser r) (rabbitPassword r)

  rabbitMq <- AMQP.openChannel rabbitMqConn

  evaluateCommand command >>= mapM_ (enqueueEmail rabbitMq)

 where

  enqueueEmail rabbitMq email =
    AMQP.publishMsg rabbitMq outboxExchange ""
      AMQP.newMsg
        { AMQP.msgBody = Aeson.encode email
        , AMQP.msgDeliveryMode = Just AMQP.Persistent
        }


--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= run

  where

    parser =
      Optparse.info (Options <$> Optparse.subparser (mconcat commands)
                             <*> rabbitOptions
                             <**> Optparse.helper)
                    mempty

    commands = [ Optparse.command "password-reset" $
                   Optparse.info (GoPasswordReset <$> dbOptions <**> Optparse.helper)
                     (Optparse.progDesc "Send mandatory password reset emails")
               ]

    dbOptions =
      PG.ConnectInfo
        <$> Optparse.strOption (mconcat [ Optparse.long "db-host"
                                        , Optparse.value "localhost"
                                        , Optparse.help "PostgreSQL database host"
                                        ])
        <*> Optparse.option (mconcat [ Optparse.long "db-port"
                                     , Optparse.value 5432
                                     , Optparse.help "PostgreSQL database port"
                                     ])
        <*> Optparse.strOption (mconcat [ Optparse.long "db-user"
                                        , Optparse.value "musicbrainz"
                                        , Optparse.help "PostgreSQL database username"
                                        ])
        <*> Optparse.strOption (mconcat [ Optparse.long "db-password"
                                        , Optparse.value ""
                                        , Optparse.help "PostgreSQL database password"
                                        ])
        <*> Optparse.strOption (mconcat [ Optparse.long "db"
                                        , Optparse.help "Name of the MusicBrainz database in PostgreSQL"
                                        , Optparse.value "musicbrainz"
                                        ])

    rabbitOptions =
      RabbitMQConnection
        <$> Optparse.strOption (mconcat [ Optparse.long "rabbit-host"
                                        , Optparse.value "127.0.0.1"
                                        , Optparse.help "RabbitMQ host"
                                        ])
        <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-vhost"
                                        , Optparse.value "/email"
                                        , Optparse.help "RabbitMQ virtual host for email"
                                        ])
        <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-user"
                                        , Optparse.value "guest"
                                        , Optparse.help "RabbitMQ username"
                                        ])
        <*> Optparse.strOption (mconcat [ Optparse.long "rabbit-password"
                                        , Optparse.value "guest"
                                        , Optparse.help "RabbitMQ username"
                                        ])
