module Data.CQRS.Memory.Internal.StorageBackend
  ( newStorageBackend
  ) where

import qualified Data.CQRS.Memory.Internal.EventStore as ES
import           Data.CQRS.Memory.Internal.Storage (Storage)
import qualified Data.CQRS.Memory.Internal.SnapshotStore as SS
import           Data.CQRS.Types.StorageBackend (StorageBackend)
import qualified Data.CQRS.Types.StorageBackend as SB
import           Data.Typeable (Typeable)

newStorageBackend :: (Show i, Ord i, Typeable i) => Storage i e -> IO (StorageBackend a i e)
newStorageBackend storage = do
  eventStore <- ES.newEventStore storage
  snapshotStore <- SS.newSnapshotStore
  pure $ SB.newStorageBackend eventStore snapshotStore
