{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Query
       ( QueryT
       , Query
       , execute
       , execute'
       , query
       , query1
       , queryAll
       , unsafeExecute
       , unsafeRunQueryT
       ) where

import           Control.Exception.Base (bracket)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift, MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Int (Int64)
import           Data.IORef (newIORef, readIORef, modifyIORef', writeIORef)
import           Database.PostgreSQL.Simple (Connection, FromRow, ToRow)
import           Database.PostgreSQL.Simple.Types (Query(..))
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Cursor as PSC
import qualified System.IO.Streams as Streams
import           System.IO.Streams.Internal (InputStream(..))
import qualified System.IO.Streams.List as SL

-- | Query monad transformer.
newtype QueryT m a = QueryT { unQueryT :: ReaderT Connection m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans QueryT where
  lift m = QueryT $ lift m

instance MonadIO m => MonadIO (QueryT m) where
  liftIO m = QueryT $ liftIO m

-- | Run query on a given 'Connection'. Performs no transaction
-- control.
unsafeRunQueryT :: Connection -> QueryT m a -> m a
unsafeRunQueryT c (QueryT q) = runReaderT q c

-- | Execute a query with no result.
execute :: ToRow p => Query -> p -> QueryT IO ()
execute q parameters = void $ execute' q parameters

-- | Execute a query an return the number of updated rows (if available).
execute' :: ToRow p => Query -> p -> QueryT IO Int64
execute' q parameters = QueryT $ do
  connection <- ask
  liftIO $ unsafeExecute connection q parameters

-- | Run a quest which is expected to return at most one result. Any
-- result rows past the first will be __ignored__.
query1 :: (ToRow p, FromRow r) => Query -> p -> QueryT IO (Maybe r)
query1 q parameters =
  query q parameters (liftIO . Streams.read)

-- | Run a query and return a list of the rows in the result. __This will read
-- ALL rows in the result into memory. It is ONLY meant for testing unless
-- you're ABSOLUTELY SURE that you won't end up using too much memory!__
queryAll :: (ToRow p, FromRow r) => Query -> p -> QueryT IO [r]
queryAll q parameters =
  query q parameters (liftIO . SL.toList)

-- | Execute query, returning the number of updated rows.
unsafeExecute :: (ToRow p) => Connection -> Query -> p -> IO Int64
unsafeExecute = PS.execute

-- | State for query execution.
data BufferState a = Done [a]
                   | Rows [a]

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result. The input stream is
-- only valid for that scope.
query :: (ToRow p, FromRow r) => Query -> p -> (InputStream r -> QueryT IO a) -> QueryT IO a
query q parameters f = QueryT $ do
  -- Get the connection
  connection <- ask
  -- Feeding function
  let feed cursor = do
        -- Start with an empty buffer.
        bufferRef <- newIORef $ Rows []
        -- Create the stream
        let !inputStream = InputStream
              (read' cursor bufferRef)
              (unread bufferRef)
        -- Give input stream to 'f'
        runReaderT (unQueryT $ f inputStream) connection
  -- Do the query subtitution
  q' <- liftIO $ Query <$> PS.formatQuery connection q parameters
  -- Run inside a bracket to ensure cursor release.
  liftIO $ bracket
    (PSC.declareCursor connection q')
    PSC.closeCursor
    feed

  where
    read' cursor bufferRef =
      readIORef bufferRef >>= \case
        Done [] ->
          return Nothing
        Done (r:rs) -> do
          writeIORef bufferRef $! Done rs
          return $ Just r
        Rows [] ->
          fetch cursor >>= \case
            [] -> do
              writeIORef bufferRef $! Done []
              return Nothing
            (r:rs) -> do
              writeIORef bufferRef $! Rows rs
              return $ Just r
        Rows (r:rs) -> do
          writeIORef bufferRef $! Rows rs
          return $ Just r

    unread bufferRef r =
      modifyIORef' bufferRef $ \case
          Done rs -> Rows (r:rs)
          Rows rs -> Rows (r:rs)

    fetch cursor =
      reverse . either id id <$>
        PSC.foldForward cursor fetchSize (\rs r -> return (r:rs)) []

    fetchSize = 256
