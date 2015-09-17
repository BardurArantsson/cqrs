{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.CQRS.Types.EventStore
       ( EventStore(..)
       , StoreError(..)
       , applyIso
       ) where

import           Control.Monad ((>=>))
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StoreError
import           Data.UUID.Types (UUID)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- | EventStore for events of type e.
data EventStore e = EventStore {
      -- | Store new events for an aggregate. May throw 'StoreError' exception
      -- if there's a problem storing the events. Guarantees atomicity, i.e.
      -- either all the events are stored, or none of them are (in the case of
      -- errors or conflicts).
      esStoreEvents :: UUID -> [PersistedEvent e] -> IO ()
    ,
      -- | Process sequence of events associated with the aggregate
      -- identified by the given UUID. Only events at or after the
      -- given version number are supplied by the input stream. The
      -- events are supplied in increasing order of version number.
      esRetrieveEvents :: forall a . UUID -> Int -> (InputStream (PersistedEvent e) -> IO a) -> IO a
    ,
      -- | Read all events from the event store. Events will be
      -- returned in order of increasing version number, grouped by
      -- aggregate UUID. __This function should ONLY be used for
      -- debugging purposes.__
      esRetrieveAllEvents :: forall a . (InputStream (UUID, PersistedEvent e) -> IO a) -> IO a
    }

-- | Transform an implementation of 'EventStore a' to an
-- implementation of 'EventStore b' via an isomorphism. This can be
-- used to add serialization/deserialization to event stores which do
-- not support storing anything other than binary data.
applyIso :: forall e' e . (e' -> e, e -> e') -> EventStore e -> EventStore e'
applyIso (f, g) (EventStore storeEvents' retrieveEvents' retrieveAllEvents') =
    EventStore storeEvents retrieveEvents retrieveAllEvents
  where
    storeEvents :: UUID -> [PersistedEvent e'] -> IO ()
    storeEvents aggregateId = storeEvents' aggregateId . map (fmap f)
    retrieveEvents :: forall a . UUID -> Int -> (InputStream (PersistedEvent e') -> IO a) -> IO a
    retrieveEvents aggregateId v0 p = retrieveEvents' aggregateId v0 $ SC.map (fmap g) >=> p
    retrieveAllEvents :: forall a. (InputStream (UUID, PersistedEvent e') -> IO a) -> IO a
    retrieveAllEvents p = retrieveAllEvents' $ SC.map (\(aggregateId, e) -> (aggregateId, fmap g e)) >=> p
