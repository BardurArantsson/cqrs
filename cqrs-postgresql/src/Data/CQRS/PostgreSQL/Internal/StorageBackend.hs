module Data.CQRS.PostgreSQL.Internal.StorageBackend
 ( newStorageBackend
 ) where

import           Data.ByteString (ByteString)
import qualified Data.CQRS.PostgreSQL.Internal.EventStore as ES
import qualified Data.CQRS.PostgreSQL.Internal.EventStream as EST
import qualified Data.CQRS.PostgreSQL.Internal.SnapshotStore as SS
import           Data.CQRS.PostgreSQL.Migrations (migrate)
import           Data.CQRS.Types.StorageBackend (StorageBackend)
import qualified Data.CQRS.Types.StorageBackend as SB
import           Database.Peregrin.Metadata (Schema)
import           Database.PostgreSQL.Simple (Connection)
import           UnliftIO.Pool (Pool, withResource)

newStorageBackend :: Pool Connection -> Schema -> IO (StorageBackend ByteString ByteString ByteString)
newStorageBackend pool schema = do
  -- Apply necessary migrations
  withResource pool $ flip migrate $ schema
  -- Initialize
  eventStore <- ES.newEventStore pool schema
  snapshotStore <- SS.newSnapshotStore pool schema
  eventStream <- EST.newEventStream pool schema
  pure $ SB.newStorageBackend eventStore snapshotStore eventStream
