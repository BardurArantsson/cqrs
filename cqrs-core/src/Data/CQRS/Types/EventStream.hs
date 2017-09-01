{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.EventStream
       ( EventStream(..)
       , transform
       ) where

import           Data.Bifunctor (bimap)
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StreamPosition
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- | EventStream for events of type 'e' identified by aggregate IDs of type 'i'.
data EventStream i e = EventStream {
      -- | Read the event stream, starting __immediately after__ the
      -- given position. The order is arbitrary-but-consistent such
      -- that all events for any given aggregate are always read in
      -- order of increasing sequence number and the ordering is
      -- stable across calls and (assuming a persistent event store)
      -- also across different runs of the program.
      esReadEventStream :: forall a. StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i e) -> IO a) -> IO a
    }

-- | Transform 'EventStream' via an isomorphism for the events and
-- aggregate IDs.
transform :: forall e e' i i' . Iso i' i -> Iso e' e -> EventStream i e -> EventStream i' e'
transform (_, gi) (_, g) (EventStream readEventStream') =
    EventStream readEventStream
  where
    readEventStream :: StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i' e') -> IO a) -> IO a
    readEventStream p' f = do
      readEventStream' p' $ \is -> do
        SC.map (\(p, e) -> (p, bimap gi g e)) is >>= f
