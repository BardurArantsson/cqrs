module Data.CQRS.PostgreSQL.Internal.StorageBackend
 ( newStorageBackend
 ) where

import           Data.ByteString (ByteString)
import qualified Data.CQRS.PostgreSQL.Internal.EventStore as ES
import qualified Data.CQRS.PostgreSQL.Internal.SnapshotStore as SS
import           Data.CQRS.Types.StorageBackend (StorageBackend)
import qualified Data.CQRS.Types.StorageBackend as SB
import           Database.Peregrin.Metadata (Schema)
import           Database.PostgreSQL.Simple (Connection)
import           UnliftIO.Pool (Pool)

newStorageBackend :: Pool Connection -> Schema -> IO (StorageBackend ByteString ByteString ByteString)
newStorageBackend pool schema = do
  eventStore <- ES.newEventStore pool schema
  snapshotStore <- SS.newSnapshotStore pool schema
  pure $ SB.newStorageBackend eventStore snapshotStore
