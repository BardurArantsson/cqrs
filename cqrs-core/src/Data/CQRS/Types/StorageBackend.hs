module Data.CQRS.Types.StorageBackend
  ( StorageBackend(..)
  , disableSnapshots
  , transformI
  , transformE
  , transformA
  , newStorageBackend
  ) where

import           Data.CQRS.Types.EventStore (EventStore)
import qualified Data.CQRS.Types.EventStore as ES
import           Data.CQRS.Types.Iso (Iso)
import           Data.CQRS.Types.SnapshotStore (SnapshotStore)
import qualified Data.CQRS.Types.SnapshotStore as SS
import Data.CQRS.SnapshotStore (nullSnapshotStore)

data StorageBackend a i e = StorageBackend {
    -- | Event store.
    sbEventStore :: EventStore i e
  ,
    -- | Snapshot store.
    sbSnapshotStore :: SnapshotStore i a
  }

-- | Make a StorageBackend.
newStorageBackend :: EventStore i e -> SnapshotStore i a -> StorageBackend a i e
newStorageBackend eventStore snapshotStore = StorageBackend eventStore snapshotStore

-- | Disable snapshots for the given 'StorageBackend'.
disableSnapshots :: StorageBackend a i e -> StorageBackend b i e
disableSnapshots (StorageBackend eventStore _) = StorageBackend eventStore nullSnapshotStore

-- | Transform the identifier type of 'StorageBackend' using an
-- isomorphism.
transformI :: Iso i' i -> StorageBackend a i e -> StorageBackend a i' e
transformI iso (StorageBackend eventStore snapshotStore) =
  StorageBackend (ES.transformI iso eventStore) (SS.transformI iso snapshotStore)

-- | Transform the event type of 'StorageBackend' using an
-- isomorphism.
transformE :: Iso e' e -> StorageBackend a i e -> StorageBackend a i e'
transformE iso (StorageBackend eventStore snapshotStore) =
  StorageBackend (ES.transformE iso eventStore) snapshotStore

-- | Transform the aggregate type of 'StorageBackend' using
-- a prism.
transformA :: (a' -> a, a -> Maybe a') -> StorageBackend a i e -> StorageBackend a' i e
transformA prism (StorageBackend eventStore snapshotStore) =
  StorageBackend eventStore (SS.transformA prism snapshotStore)
