module Data.CQRS.Internal.Aggregate
       ( Aggregate
       , aggregateSnapshotVersion
       , aggregateSnapshot
       , aggregateValue
       , aggregateVersion0
       , applyEvent
       , applySnapshot
       , emptyAggregate
       , versionedEvents
       , publishEvent
       ) where

import           Control.DeepSeq (NFData, ($!!))
import           Data.CQRS.Types.AggregateAction (AggregateAction)
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import qualified Data.Foldable as F
import           Data.Int (Int32, Int64)
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import           Data.Typeable (Typeable)

-- Aggregate's in-memory state.
data Aggregate a e = Aggregate
    { aggregateAction :: AggregateAction a e
    , aggregateValue :: Maybe a
    , aggregateEvents :: Seq (e, Int64)
    , aggregateVersion0 :: {-# UNPACK #-} !Int32
    , aggregateSnapshotVersion :: {-# UNPACK #-} !Int32
    } deriving (Typeable)

-- Make "empty" aggregate for applying snapshots and events to.
emptyAggregate :: AggregateAction a e -> Aggregate a e
emptyAggregate aggregateAction' =
    Aggregate { aggregateAction = aggregateAction'
                   , aggregateValue = Nothing
                   , aggregateEvents = S.empty
                   , aggregateVersion0 = -1
                   , aggregateSnapshotVersion = -1
                   }

-- Apply snapshot to aggregate.
applySnapshot :: Aggregate a e -> Maybe (Snapshot a) -> Aggregate a e
applySnapshot a0 Nothing = a0
applySnapshot a0 (Just s) =
  a0 { aggregateValue = Just $ sSnapshot s
     , aggregateVersion0 = sVersion s
     , aggregateSnapshotVersion = sVersion s }

-- Apply event from event store to aggregate. The event will not be published.
applyEvent :: Aggregate a e -> PersistedEvent i e -> Aggregate a e
applyEvent a pe =
    a { aggregateValue = Just $ aggregateAction a (aggregateValue a) $ peEvent pe
      , aggregateVersion0 = max (peSequenceNumber pe) (aggregateVersion0 a)
      }

-- Publish event to aggregate.
publishEvent :: (NFData e, NFData a) => Aggregate a e -> e -> Int64 -> Aggregate a e
publishEvent a e ts =
    a { aggregateValue = Just $!! aggregateAction a (aggregateValue a) e
      , aggregateEvents = (|>) (aggregateEvents a) $!! (e, ts)
      }

-- Return events with attached version numbers.
versionedEvents :: Aggregate a e -> [(Int32, (e, Int64))]
versionedEvents a = zip [v0+1 ..] evs
  where
    evs = F.toList $ aggregateEvents a
    v0 = aggregateVersion0 a

-- Return a snapshot of the aggregate state.
aggregateSnapshot :: Aggregate a e -> Maybe (Int32, a)
aggregateSnapshot a = fmap (\av -> (v, av)) (aggregateValue a)
  where
    v = fromIntegral (S.length $ aggregateEvents a) + aggregateVersion0 a
