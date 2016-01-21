module Data.CQRS.PostgreSQL
    ( newEventStore
    , newSnapshotStore
    ) where

import Data.CQRS.PostgreSQL.Internal.EventStore (newEventStore)
import Data.CQRS.PostgreSQL.Internal.SnapshotStore (newSnapshotStore)
