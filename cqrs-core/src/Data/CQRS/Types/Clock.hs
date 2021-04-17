module Data.CQRS.Types.Clock
       ( Clock
       , autoIncrementingClock
       , getMillis
       , operatingSystemClock
       ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import           Data.Int (Int64)
import           Data.IORef
import           Data.Time.Clock.POSIX (getPOSIXTime)

-- | Clock which can return the current system time.
newtype Clock = Clock (IO Int64)

-- | Read the system clock. Returns the current system time
-- in units of milliseconds since the epoch.
getMillis :: MonadUnliftIO m => Clock -> m Int64
getMillis (Clock get) = liftIO get

-- | Create a 'Clock' from an arbitrary IO action
-- returning the number of elapsed milliseconds since the epoch.
mkClock :: IO Int64 -> Clock
mkClock = Clock

-- | A 'Clock' which returns the current system time from the
-- operating system. The resolution of the clock is determined by the
-- operating system.
operatingSystemClock :: Clock
operatingSystemClock = mkClock get
  where
    get = do
      t <- getPOSIXTime
      return $ round (t * 1000)

-- | Create an auto-incrementing 'Clock'. For example,
-- |autoIncrementingClock 42 3| creates a 'Clock' which starts at the
-- value '42' and increments the returned time by '3' every time it is
-- called.
--
-- This is intended for testing.
autoIncrementingClock :: Int64 -> Int64 -> IO Clock
autoIncrementingClock start increment = do
  value <- newIORef start
  return $ mkClock $
    atomicModifyIORef' value (\ts -> (ts + increment, ts))
