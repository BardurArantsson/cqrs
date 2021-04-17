{-# LANGUAGE RankNTypes #-}
module Data.CQRS.View.Types.EventSource
  ( EventSource(..)
  , mkEmpty
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.CQRS.Types.Chunk (Chunk)
import           Data.CQRS.Types.EventStream (EventStream)
import qualified Data.CQRS.Types.EventStream as EventStream
import           Data.CQRS.Types.PersistedEvent (PersistedEvent)
import           UnliftIO.Streams (InputStream, nullInput)

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

-- | Empty event source, i.e. a source which has no events nor aggregates.
mkEmpty :: MonadUnliftIO m => m (EventSource i e)
mkEmpty = do
  i1 <- nullInput
  es <- EventStream.mkEmpty
  pure $ EventSource
    { esFindByAggregateId = \_ f -> f i1
    , esSubscribe = \_ -> pure ()
    , esEventStream = es
    }
