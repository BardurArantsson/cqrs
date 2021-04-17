{-# LANGUAGE RankNTypes #-}
module Data.CQRS.View.Types.EventSource
  ( EventSource(..)
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.CQRS.Types.Chunk (Chunk)
import           Data.CQRS.Types.EventStream (EventStream)
import           Data.CQRS.Types.PersistedEvent (PersistedEvent)
import           UnliftIO.Streams (InputStream)

-- | A source of events for aggregates used for building views.
data EventSource i e = EventSource {

    -- | Read all events for a given aggregate.
    esFindByAggregateId :: forall m a . (MonadUnliftIO m) => i -> (InputStream (PersistedEvent e) -> m a) -> m a
  ,
    -- | Subscribe to push-based events.
    esSubscribe :: forall m . (MonadUnliftIO m) => (Chunk i e -> m ()) -> m ()

  ,
    -- | Event stream
    esEventStream :: EventStream i e

}
