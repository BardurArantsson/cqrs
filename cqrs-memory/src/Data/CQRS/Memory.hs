-- | Memory-based event store. Used primarily for testing.
module Data.CQRS.Memory
    ( Storage
    , newEventStore
    , newStorage
    , newSnapshotStore
    ) where

import Data.CQRS.Memory.Internal.EventStore
import Data.CQRS.Memory.Internal.Storage
import Data.CQRS.Memory.Internal.SnapshotStore
