{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

--------------------------------------------------------------------------------
import Options.Applicative ((<**>))

#if (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid (mconcat, mempty)
#endif

--------------------------------------------------------------------------------
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

    commands = [ Optparse.command "retry" $
                   Optparse.info (Enqueue.Retry <$> Optparse.subparser (mconcat retryCommand)
                                                <**> Optparse.helper)
                     (Optparse.progDesc "Retry sending failed or unroutable emails")
               ]

    retryCommand = [ Optparse.command "unroutable" $
                       Optparse.info (pure [Enqueue.Unroutable] <**> Optparse.helper)
                         (Optparse.progDesc "Retry all unroutable emails")
                   , Optparse.command "invalid" $
                       Optparse.info (pure [Enqueue.Invalid] <**> Optparse.helper)
                         (Optparse.progDesc "Retry all invalid emails")
                   , Optparse.command "everything" $
                       Optparse.info (pure [Enqueue.Invalid, Enqueue.Unroutable]
                                        <**> Optparse.helper)
                         (Optparse.progDesc "Retry all invalid or unroutable emails")
                   ]
