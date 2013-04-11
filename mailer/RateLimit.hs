module RateLimit (rateLimit) where

--------------------------------------------------------------------------------
import Control.Applicative ((<*))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad (void)


--------------------------------------------------------------------------------
import qualified Data.Time as Time


--------------------------------------------------------------------------------
-- | Given a rate, which is the maximum allowable throughput per second, this
-- will produce a rate limiting action, which when invoked will cause the
-- calling thread to sleep for the minimum duration required to adhere to the
-- requested rate limit.
rateLimit :: Time.NominalDiffTime -> (a -> IO b) -> IO (a -> IO b)
rateLimit rate action = do
  lastSent <- Time.getCurrentTime >>= newMVar

  return $ \a -> do
    now <- Time.getCurrentTime
    lastSentAt <- readMVar lastSent

    let delay = max 0 ((1 / rate) - (now `Time.diffUTCTime` lastSentAt))
    threadDelay (floor $ delay * 1000000)

    action a <* updateLastSent lastSent

 where
  updateLastSent lastTime = void $ Time.getCurrentTime >>= swapMVar lastTime
