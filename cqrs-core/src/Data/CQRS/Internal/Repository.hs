{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Internal.Repository
    ( Repository(..)
    , Settings(..)
    , setSnapshotFrequency
    , setClock
    , defaultSettings
    , newRepository
    ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.CQRS.Types.AggregateAction
import           Data.CQRS.Types.Chunk
import           Data.CQRS.Types.Clock
import           Data.CQRS.Types.StorageBackend

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
    , repositoryStorageBackend :: StorageBackend a i e
    , repositoryPublishEvents :: forall m . (MonadUnliftIO m) => Chunk i e -> m ()
    , repositorySettings :: Settings
    }

-- | Create a repository.
newRepository :: forall i a e . Settings -> AggregateAction a e -> StorageBackend a i e -> (forall m . (MonadUnliftIO m) => Chunk i e -> m ()) -> Repository i a e
newRepository settings aggregateAction storageBackend publishEvents =
  Repository aggregateAction storageBackend publishEvents settings
