module RateLimit (rateLimiter) where

--------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, withMVar)
import Control.Exception (evaluate)
import Control.Monad (join, void)


--------------------------------------------------------------------------------
import qualified Data.Time as Time


--------------------------------------------------------------------------------
-- | Given a rate, which is the maximum allowable throughput per second, this
-- will produce a rate limiting action, which when invoked will cause the
-- calling thread to sleep for the minimum duration required to adhere to the
-- requested rate limit.
rateLimiter :: Time.NominalDiffTime -> IO (IO ())
rateLimiter rate = do
  lastSent <- Time.getCurrentTime >>= newMVar

  return $ do
    now <- Time.getCurrentTime
    lastSentAt <- readMVar lastSent

    threadDelay $ floor $
      max 0 ((1 / rate) - (now `Time.diffUTCTime` lastSentAt)) * 1000000

    updateLastSent lastSent

 where
  updateLastSent lastTime = void $
    withMVar lastTime (const $ join $ evaluate Time.getCurrentTime)
