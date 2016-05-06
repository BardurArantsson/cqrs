module Data.CQRS.PostgreSQL
    ( newEventStore
    , newEventStream
    , newSnapshotStore
    ) where

THIS_IS_AN_ERROR

import Data.CQRS.PostgreSQL.Internal.EventStore (newEventStore)
import Data.CQRS.PostgreSQL.Internal.EventStream (newEventStream)
import Data.CQRS.PostgreSQL.Internal.SnapshotStore (newSnapshotStore)
