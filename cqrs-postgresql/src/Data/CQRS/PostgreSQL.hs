module Data.CQRS.PostgreSQL
    ( Schema(..)
    , newEventStore
    , newEventStream
    , newKVStore
    , newSnapshotStore
    , newStorageBackend
    ) where

import Data.CQRS.PostgreSQL.Internal.EventStore (newEventStore)
import Data.CQRS.PostgreSQL.Internal.EventStream (newEventStream)
import Data.CQRS.PostgreSQL.Internal.KVStore (newKVStore)
import Data.CQRS.PostgreSQL.Internal.SnapshotStore (newSnapshotStore)
import Data.CQRS.PostgreSQL.Internal.StorageBackend (newStorageBackend)
import Database.Peregrin.Metadata (Schema(..))
