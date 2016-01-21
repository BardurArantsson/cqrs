{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.CQRS.Types.EventStream
       ( EventStream(..)
       , StreamPosition  -- Re-export
       , transform
       ) where

import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StreamPosition
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- | EventStream for events of type 'e' identified by aggregate IDs of type 'i'.
data EventStream i e = EventStream {
      -- | Read the event stream, starting __immediately after__ the
      -- given position. If no starting position is given, the
      -- function starts reading from the very start of the event
      -- stream. The order is arbitrary-but-consistent such that all
      -- events for any given aggregate are always read in order of
      -- increasing sequence number and the ordering is stable across
      -- calls and (assuming a persistent event store) also across
      -- different runs of the program.
      esReadEventStream :: forall a. Maybe StreamPosition -> (InputStream (StreamPosition, i, PersistedEvent e) -> IO a) -> IO a
    }

-- | Transform 'EventStream' via an isomorphism for the events and
-- aggregate IDs.
transform :: forall e e' i i' . Iso e' e -> Iso i' i -> EventStream i e -> EventStream i' e'
transform (_, g) (_, gi) (EventStream readEventStream') =
    EventStream readEventStream
  where
    readEventStream :: Maybe StreamPosition -> (InputStream (StreamPosition, i', PersistedEvent e') -> IO a) -> IO a
    readEventStream p' f = do
      readEventStream' p' $ \is -> do
        SC.map (\(p, i, e) -> (p, gi i, fmap g e)) is >>= f
