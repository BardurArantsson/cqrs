module Data.CQRS.Internal.Repository
    ( Repository(..)
    , Settings(..)
    , setSnapshotFrequency
    , setClock
    , defaultSettings
    , newRepository
    ) where

import           Control.Monad (void)
import           Data.CQRS.Types.AggregateAction
import           Data.CQRS.Types.Chunk
import           Data.CQRS.Types.Clock
import           Data.CQRS.Types.EventStore
import           Data.CQRS.Types.SnapshotStore

-- | Repository settings
data Settings = Settings
    { settingsSnapshotFrequency :: Maybe Int
    , settingsClock :: Clock
    }

-- | Default repository settings:
--
--     * There are no snapshots by default
--
defaultSettings :: Settings
defaultSettings = Settings
    { settingsSnapshotFrequency = Nothing
    , settingsClock = operatingSystemClock
    }

-- | Set the snapshot frequency. 0 or negative means
-- that no snapshots will be written. Any existing snapshots
-- will still be used.
setSnapshotFrequency :: Int -> Settings -> Settings
setSnapshotFrequency n s = s { settingsSnapshotFrequency = n' }
  where n' | n <= 0    = Nothing
           | otherwise = Just n

-- | Set the system clock to use.
setClock :: Clock -> Settings -> Settings
setClock clock s = s { settingsClock = clock }

-- | Repository consisting of an event store and an event bus.
data Repository i a e = Repository
    { repositoryAggregateAction :: AggregateAction a e
    , repositoryEventStore :: EventStore i e
    , repositorySnapshotStore :: SnapshotStore i a
    , repositoryPublishEvents :: Chunk i e -> IO ()
    , repositorySettings :: Settings
    }

-- | Create a repository.
newRepository :: Settings -> AggregateAction a e -> EventStore i e -> SnapshotStore i a -> (Chunk i e -> IO r) -> Repository i a e
newRepository settings aggregateAction eventStore snapshotStore publishEvents = do
  Repository aggregateAction eventStore snapshotStore publishEvents' settings
  where
    publishEvents' inputStream = void $ publishEvents inputStream
