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
import           Data.CQRS.Types.SnapshotStore
import           Data.CQRS.Types.AggregateAction (AggregateAction)
import           Data.UUID.Types (UUID)

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
data Repository a e = Repository
    { repositoryAggregateAction :: AggregateAction a e
    , repositoryEventStore :: EventStore e
    , repositorySnapshotStore :: SnapshotStore a
    , repositoryPublishEvents :: (UUID, [PersistedEvent e]) -> IO ()
    , repositoryUUIDSupply :: IO UUID
    , repositorySettings :: Settings
    }

-- | Create a repository from a pool of event store backends.
newRepository :: (Show e, NFData e) => Settings -> AggregateAction a e -> EventStore e -> SnapshotStore a -> ((UUID, [PersistedEvent e]) -> IO r) -> IO UUID -> (Repository a e)
newRepository settings aggregateAction eventStore snapshotStore publishEvents uuidSupply = do
  Repository aggregateAction eventStore snapshotStore publishEvents' uuidSupply settings
  where
    publishEvents' inputStream = void $ publishEvents inputStream
