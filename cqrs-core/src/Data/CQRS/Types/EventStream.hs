{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.EventStream
       ( EventStream(..)
       , transform
       ) where

import           Control.Arrow (second)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.Bifunctor (bimap)
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StreamPosition
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.Combinators as SC

-- | EventStream for events of type 'e' identified by aggregate IDs of type 'i'.
newtype EventStream i e = EventStream {
      -- | Read the event stream, starting __immediately after__ the
      -- given position. The order is arbitrary-but-consistent such
      -- that all events for any given aggregate are always read in
      -- order of increasing sequence number and the ordering is
      -- stable across calls and (assuming a persistent event store)
      -- also across different runs of the program.
      esReadEventStream :: forall a m . (MonadUnliftIO m) => StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i e) -> m a) -> m a
    }

-- | Transform 'EventStream' via an isomorphism for the events and
-- aggregate IDs.
transform :: forall e e' i i' . Iso i' i -> Iso e' e -> EventStream i e -> EventStream i' e'
transform (_, gi) (_, g) (EventStream readEventStream') =
    EventStream readEventStream
  where
    readEventStream :: MonadUnliftIO m' => StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i' e') -> m' a) -> m' a
    readEventStream p' f = readEventStream' p' $ SC.map (second $ bimap gi g) >=> f
