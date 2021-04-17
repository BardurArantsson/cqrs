module Data.CQRS.Types.StorageBackend
  ( StorageBackend(..)
  , disableSnapshots
  , transformI
  , transformE
  , transformA
  , newStorageBackend
  ) where

import           Data.CQRS.SnapshotStore (nullSnapshotStore)
import           Data.CQRS.Types.EventStore (EventStore)
import qualified Data.CQRS.Types.EventStore as ES
import           Data.CQRS.Types.EventStream (EventStream)
import qualified Data.CQRS.Types.EventStream as EST
import           Data.CQRS.Types.Iso (Iso)
import           Data.CQRS.Types.SnapshotStore (SnapshotStore)
import qualified Data.CQRS.Types.SnapshotStore as SS

data StorageBackend a i e = StorageBackend {
    -- | Event store.
    sbEventStore :: EventStore i e
  ,
    -- | Snapshot store.
    sbSnapshotStore :: SnapshotStore i a
  ,
    -- | Event stream.
    sbEventStream :: EventStream i e

  }

-- | Make a StorageBackend.
newStorageBackend :: EventStore i e -> SnapshotStore i a -> EventStream i e -> StorageBackend a i e
newStorageBackend = StorageBackend

-- | Disable snapshots for the given 'StorageBackend'.
disableSnapshots :: StorageBackend a i e -> StorageBackend b i e
disableSnapshots (StorageBackend eventStore _ eventStream) = StorageBackend eventStore nullSnapshotStore eventStream

-- | Transform the identifier type of 'StorageBackend' using an
-- isomorphism.
transformI :: Iso i' i -> StorageBackend a i e -> StorageBackend a i' e
transformI iso (StorageBackend eventStore snapshotStore eventStream) =
  StorageBackend (ES.transformI iso eventStore) (SS.transformI iso snapshotStore) (EST.transformI iso eventStream)

-- | Transform the event type of 'StorageBackend' using an
-- isomorphism.
transformE :: Iso e' e -> StorageBackend a i e -> StorageBackend a i e'
transformE iso (StorageBackend eventStore snapshotStore eventStream) =
  StorageBackend (ES.transformE iso eventStore) snapshotStore (EST.transformE iso eventStream)

-- | Transform the aggregate type of 'StorageBackend' using
-- a prism.
transformA :: (a' -> a, a -> Maybe a') -> StorageBackend a i e -> StorageBackend a' i e
transformA prism (StorageBackend eventStore snapshotStore eventStream) =
  StorageBackend eventStore (SS.transformA prism snapshotStore) eventStream
