module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>), (<**>), pure)
import Data.Monoid (mconcat, mempty)


--------------------------------------------------------------------------------
import qualified Options.Applicative as Optparse


--------------------------------------------------------------------------------
import qualified MusicBrainz.Messaging as Messaging

import qualified Retry


--------------------------------------------------------------------------------
main :: IO ()
main = Optparse.execParser parser >>= Retry.retry

 where

  parser =
    Optparse.info
      (Retry.Options
        <$> Messaging.rabbitOptparse
        <*> (Optparse.subparser (mconcat commands) <**> Optparse.helper))
       mempty

  commands = [ Optparse.command "unroutable" $
                 Optparse.info (pure [Retry.Unroutable] <**> Optparse.helper)
                   (Optparse.progDesc "Retry all unroutable emails")
             , Optparse.command "invalid" $
                 Optparse.info (pure [Retry.Invalid] <**> Optparse.helper)
                   (Optparse.progDesc "Retry all invalid emails")
             , Optparse.command "everything" $
                 Optparse.info (pure [Retry.Invalid, Retry.Unroutable]
                                  <**> Optparse.helper)
                   (Optparse.progDesc "Retry all invalid or unroutable emails")
             ]
