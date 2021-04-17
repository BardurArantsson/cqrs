module Data.CQRS.PostgreSQL
    ( Schema(..)
    , newEventStore
    , newEventStream
    , newKVStore
    , newSnapshotStore
    ) where

import Data.CQRS.PostgreSQL.Internal.EventStore (newEventStore)
import Data.CQRS.PostgreSQL.Internal.EventStream (newEventStream)
import Data.CQRS.PostgreSQL.Internal.KVStore (newKVStore)
import Data.CQRS.PostgreSQL.Internal.SnapshotStore (newSnapshotStore)
import Database.Peregrin.Metadata (Schema(..))
