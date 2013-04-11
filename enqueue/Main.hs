{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>), (<**>))
import Data.Monoid (mconcat, mempty)


--------------------------------------------------------------------------------
import qualified Database.PostgreSQL.Simple as PG
import qualified Options.Applicative as Optparse


--------------------------------------------------------------------------------
import qualified Enqueue
import qualified MusicBrainz.Messaging as Messaging

--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= Enqueue.run

  where

    parser =
      Optparse.info
        (Enqueue.Options <$> Optparse.subparser (mconcat commands)
                         <*> Messaging.rabbitOptparse
                         <**> Optparse.helper)
                    mempty

    commands = [ Optparse.command "password-reset" $
                   Optparse.info (Enqueue.PasswordReset <$> dbOptions <**> Optparse.helper)
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
