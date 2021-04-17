{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.EventStore
       ( EventStore(..)
       , StoreError(..)
       , transform
       , transformI
       , transformE
       ) where

import           Control.Monad ((>=>))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.Bifunctor (first, second)
import           Data.CQRS.Types.Chunk
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StoreError
import           Data.Int (Int32)
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.Combinators as SC

-- | EventStore for events of type 'e' applied to aggregates with
-- identifiers of type 'i'.
data EventStore i e = EventStore {
      -- | Store new events for an aggregate. May throw 'StoreError' exception
      -- if there's a problem storing the events. Guarantees atomicity, i.e.
      -- either all the events are stored, or none of them are (in the case of
      -- errors or conflicts). All given events __MUST__ have the same aggregate
      -- ID as specified in the first parameter.
      esStoreEvents :: forall m . (MonadUnliftIO m) => Chunk i e -> m ()
    ,
      -- | Read sequence of events associated with the given aggregate.
      -- Only events at or after the given version number are supplied
      -- by the input stream. The events are supplied in increasing
      -- order of version number.
      esRetrieveEvents :: forall m a . (MonadUnliftIO m) => i -> Int32 -> (InputStream (PersistedEvent e) -> m a) -> m a
    ,
      -- | Read all events from the event store. Events will be
      -- returned in order of increasing version number, grouped by
      -- aggregate ID. __This function should ONLY be used for
      -- debugging purposes.__
      esRetrieveAllEvents :: forall m a . (MonadUnliftIO m) => (InputStream (PersistedEvent' i e) -> m a) -> m a
    }

-- | Transform an implementation of 'EventStore i a' to an
-- implementation of 'EventStore j b' via two isomorphisms. This can
-- be used to add serialization/deserialization to event stores which
-- do not support storing anything other than binary data.
transform :: forall e' e i' i . Iso i' i -> Iso e' e -> EventStore i e -> EventStore i' e'
transform ifg efg eventStore =
  transformI ifg $ transformE efg eventStore

-- | Transform an 'EventStore i e' to an 'EventStore j e' using
-- an isomorphism between 'i' and 'j'.
transformI :: forall i' i e . Iso i' i -> EventStore i e -> EventStore i' e
transformI (fi, gi) (EventStore storeEvents retrieveEvents retrieveAllEvents) =
    EventStore storeEvents' retrieveEvents' retrieveAllEvents'
  where
    storeEvents' :: forall m' . (MonadUnliftIO m') => Chunk i' e -> m' ()
    storeEvents' = storeEvents . first fi

    retrieveEvents' :: forall a m' . (MonadUnliftIO m') => i' -> Int32 -> (InputStream (PersistedEvent e) -> m' a) -> m' a
    retrieveEvents' aggregateId' v0 p' =
      retrieveEvents (fi aggregateId') v0 p'

    retrieveAllEvents' :: forall a m' . (MonadUnliftIO m') => (InputStream (PersistedEvent' i' e) -> m' a) -> m' a
    retrieveAllEvents' p' =
      retrieveAllEvents $ SC.map (first gi) >=> p'

-- | Transform an 'EventStore i e' to an 'EventStore i f' using
-- an isomorphism between 'e' and 'f'.
transformE :: forall e' e i . Iso e' e -> EventStore i e -> EventStore i e'
transformE (fe, ge) (EventStore storeEvents retrieveEvents retrieveAllEvents) =
    EventStore storeEvents' retrieveEvents' retrieveAllEvents'
  where
    storeEvents' :: forall m' . (MonadUnliftIO m') => Chunk i e' -> m' ()
    storeEvents' = storeEvents . second fe

    retrieveEvents' :: forall a m' . (MonadUnliftIO m') => i -> Int32 -> (InputStream (PersistedEvent e') -> m' a) -> m' a
    retrieveEvents' aggregateId v0 p' =
      retrieveEvents aggregateId v0 $ SC.map (fmap ge) >=> p'

    retrieveAllEvents' :: forall a m' . (MonadUnliftIO m') => (InputStream (PersistedEvent' i e') -> m' a) -> m' a
    retrieveAllEvents' p' =
      retrieveAllEvents $ SC.map (second ge) >=> p'
