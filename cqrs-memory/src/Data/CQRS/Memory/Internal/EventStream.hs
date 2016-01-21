{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Memory.Internal.EventStream
    ( StreamPosition
    , newEventStream
    ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (liftM)
import           Data.CQRS.Types.EventStream
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Internal.StreamPosition
import           Data.CQRS.Memory.Internal.Storage
import qualified Data.Foldable as F
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import qualified System.IO.Streams.List as SL

readEventStream :: forall i e a . Storage i e -> Maybe StreamPosition -> (InputStream (StreamPosition, i, PersistedEvent e) -> IO a) -> IO a
readEventStream (Storage store) p0 f = do
    -- Take a snapshot of all the events in the store
    allEvents <- liftM msEvents $ readTVarIO store
    -- What is the starting position?
    let i0 = case p0 of
               Just (StreamPosition i) -> i
               Nothing                 -> 0 -- Beginning of time
    -- Filter out irrelevant events
    let events = filter (\(Event _ _ i) -> i > i0) $ F.toList allEvents
    -- Start streaming events.
    SL.fromList events >>= SC.map reformat >>= f
  where
    reformat :: Event i e -> (StreamPosition, i, PersistedEvent e)
    reformat (Event i e p) = (StreamPosition p, i, e)

newEventStream :: Storage i e -> IO (EventStream i e)
newEventStream storage = do
  return $ EventStream
    { esReadEventStream = readEventStream storage
    }
