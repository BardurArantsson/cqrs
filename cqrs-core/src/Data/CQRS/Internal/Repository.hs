module Data.CQRS.Internal.Repository
    ( Repository(..)
    , Settings(..)
    , setSnapshotFrequency
    , defaultSettings
    , newRepository
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (void)
import           Data.CQRS.Types.EventStore (EventStore(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent)
import           Data.CQRS.Types.SnapshotStore (SnapshotStore)
import           Data.CQRS.Types.AggregateAction (AggregateAction)

-- | Repository settings
data Settings = Settings
    { settingsSnapshotFrequency :: Maybe Int
    }

-- | Default repository settings:
--
--     * There are no snapshots by default
--
defaultSettings :: Settings
defaultSettings = Settings
    { settingsSnapshotFrequency = Nothing
    }

-- | Set the snapshot frequency. 0 or negative means
-- that no snapshots will be written. Any existing snapshots
-- will still be used.
setSnapshotFrequency :: Int -> Settings -> Settings
setSnapshotFrequency n s = s { settingsSnapshotFrequency = n' }
  where n' | n <= 0    = Nothing
           | otherwise = Just n

-- | Repository consisting of an event store and an event bus.
data Repository i a e = Repository
    { repositoryAggregateAction :: AggregateAction a e
    , repositoryEventStore :: EventStore i e
    , repositorySnapshotStore :: SnapshotStore i a
    , repositoryPublishEvents :: (i, [PersistedEvent e]) -> IO ()
    , repositorySettings :: Settings
    }

-- | Create a repository from a pool of event store backends.
newRepository :: (Show e, NFData e) => Settings -> AggregateAction a e -> EventStore i e -> SnapshotStore i a -> ((i, [PersistedEvent e]) -> IO r) -> Repository i a e
newRepository settings aggregateAction eventStore snapshotStore publishEvents = do
  Repository aggregateAction eventStore snapshotStore publishEvents' settings
  where
    publishEvents' inputStream = void $ publishEvents inputStream
