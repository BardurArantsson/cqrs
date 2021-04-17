{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.EventStream
       ( EventStream(..)
       , mkEmpty
       , transform
       , transformI
       , transformE
       ) where

import           Control.Arrow (second)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.Bifunctor (first)
import qualified Data.Bifunctor as B
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StreamPosition
import           Data.Int (Int32)
import           UnliftIO.Streams (InputStream, nullInput)
import qualified UnliftIO.Streams.Combinators as SC

-- | EventStream for events of type 'e' identified by aggregate IDs of type 'i'.
data EventStream i e = EventStream {
      -- | Read the event stream, starting __immediately after__ the
      -- given position. The order is arbitrary-but-consistent such
      -- that all events for any given aggregate are always read in
      -- order of increasing sequence number and the ordering is
      -- stable across calls and (assuming a persistent event store)
      -- also across different runs of the program.
      esReadEventStream :: forall a m . (MonadUnliftIO m) => StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i e) -> m a) -> m a

    ,

      -- | Read events for a given aggregate and from a given sequence
      -- number (inclusive). The events are streamed in order of
      -- increasing sequence number.
      esReadAggregateEvents :: forall a m . (MonadUnliftIO m) => i -> Int32 -> (InputStream (PersistedEvent e) -> m a) -> m a

    }

-- | Transform 'EventStream' via an isomorphism for the events and
-- aggregate IDs.
transform :: forall e e' i i' . Iso i' i -> Iso e' e -> EventStream i e -> EventStream i' e'
transform ifg efg eventStream = transformI ifg $ transformE efg eventStream

-- | Transform an 'EventStream i e' to an 'EventStream j e' via
-- an isomorphism between 'i' and 'j'.
transformI :: forall e i i' . Iso i' i -> EventStream i e -> EventStream i' e
transformI (fi, gi) (EventStream readEventStream' readAggregateEvents') =
    EventStream readEventStream readAggregateEvents
  where
    readEventStream :: MonadUnliftIO m' => StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i' e) -> m' a) -> m' a
    readEventStream p' f =
      readEventStream' p' $ SC.map (second $ first gi) >=> f

    readAggregateEvents :: forall a m' . (MonadUnliftIO m') => i' -> Int32 -> (InputStream (PersistedEvent e) -> m' a) -> m' a
    readAggregateEvents aggregateId' v0 p' =
      readAggregateEvents' (fi aggregateId') v0 p'

-- | Transform an 'EventStream i e' to an 'EventStream i f' via
-- an isomorphism between 'e' and 'f'.
transformE :: forall i e e' . Iso e' e -> EventStream i e -> EventStream i e'
transformE (_, ge) (EventStream readEventStream' readAggregateEvents') =
    EventStream readEventStream readAggregateEvents
  where
    readEventStream :: MonadUnliftIO m' => StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i e') -> m' a) -> m' a
    readEventStream p' f =
      readEventStream' p' $ SC.map (second $ B.second ge) >=> f

    readAggregateEvents :: forall a m' . (MonadUnliftIO m') => i -> Int32 -> (InputStream (PersistedEvent e') -> m' a) -> m' a
    readAggregateEvents aggregateId v0 p' =
      readAggregateEvents' aggregateId v0 $ SC.map (fmap ge) >=> p'

-- | Create an empty event stream.
mkEmpty :: MonadUnliftIO m => m (EventStream i e)
mkEmpty = do
  i0 <- nullInput
  i1 <- nullInput
  pure $ EventStream
    { esReadEventStream = \_ f -> f i0
    , esReadAggregateEvents = \_ _ f -> f i1
    }
