-- | Aggregate action type definition.
module Data.CQRS.Types.AggregateAction
       ( AggregateAction
       ) where

-- | An aggregate action is just a function for applying an event to
-- an aggregate. Aggregates that have not been created yet will be
-- passed in as @Nothing@ and aggregates which are being updated
-- will be passed in as @Just x@.
type AggregateAction a e = (Maybe a -> e -> a)
