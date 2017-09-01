{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.EventStore
       ( EventStore(..)
       , StoreError(..)
       , transform
       ) where

import           Control.Monad ((>=>))
import           Data.Bifunctor (bimap)
import           Data.CQRS.Types.Chunk
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StoreError
import           Data.Int (Int32)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- | EventStore for events of type e.
data EventStore i e = EventStore {
      -- | Store new events for an aggregate. May throw 'StoreError' exception
      -- if there's a problem storing the events. Guarantees atomicity, i.e.
      -- either all the events are stored, or none of them are (in the case of
      -- errors or conflicts). All given events __MUST__ have the same aggregate
      -- ID as specified in the first parameter.
      esStoreEvents :: Chunk i e -> IO ()
    ,
      -- | Process sequence of events associated with the given aggregate.
      -- Only events at or after the given version number are supplied
      -- by the input stream. The events are supplied in increasing
      -- order of version number.
      esRetrieveEvents :: forall a . i -> Int32 -> (InputStream (PersistedEvent i e) -> IO a) -> IO a
    ,
      -- | Read all events from the event store. Events will be
      -- returned in order of increasing version number, grouped by
      -- aggregate ID. __This function should ONLY be used for
      -- debugging purposes.__
      esRetrieveAllEvents :: forall a . (InputStream (PersistedEvent' i e) -> IO a) -> IO a
    }

-- | Transform an implementation of 'EventStore i a' to an
-- implementation of 'EventStore j b' via two isomorphisms. This can
-- be used to add serialization/deserialization to event stores which
-- do not support storing anything other than binary data.
transform :: forall e' e i' i . Iso i' i -> Iso e' e -> EventStore i e -> EventStore i' e'
transform (fi, gi) (fe, ge) (EventStore storeEvents retrieveEvents retrieveAllEvents) =
    EventStore storeEvents' retrieveEvents' retrieveAllEvents'
  where
    storeEvents' :: Chunk i' e' -> IO ()
    storeEvents' = storeEvents . bimap fi fe

    retrieveEvents' :: forall a . i' -> Int32 -> (InputStream (PersistedEvent i' e') -> IO a) -> IO a
    retrieveEvents' aggregateId' v0 p' =
      -- To avoid redundant conversions, we 'map' the event aggregate
      -- IDs by simply replacing them. This is valid since the
      -- contract specifies that they must all equal the given
      -- 'aggregateID' parameter. This is strictly a performance
      -- optimization.
      retrieveEvents (fi aggregateId') v0 $ SC.map (bimap (const aggregateId') ge) >=> p'

    retrieveAllEvents' :: forall a. (InputStream (PersistedEvent' i' e') -> IO a) -> IO a
    retrieveAllEvents' p' =
      retrieveAllEvents $ SC.map (bimap gi ge) >=> p'
