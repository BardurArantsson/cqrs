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

import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift, MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Int (Int64)
import           Database.PostgreSQL.Simple (Connection, FromRow, ToRow)
import           Database.PostgreSQL.Simple.Types (Query(..))
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple.Cursor (Cursor)
import qualified Database.PostgreSQL.Simple.Cursor as PSC
import qualified System.IO.Streams as Streams
import           System.IO.Streams.Internal (InputStream(..))
import qualified System.IO.Streams.List as SL
import           UnliftIO (MonadIO(..), MonadUnliftIO(..), UnliftIO(..), liftIO, bracket, withUnliftIO)
import           UnliftIO.IORef (newIORef, readIORef, modifyIORef', writeIORef)

-- | Query monad transformer.
newtype QueryT m a = QueryT { unQueryT :: ReaderT Connection m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans QueryT where
  lift m = QueryT $ lift m

instance MonadIO m => MonadIO (QueryT m) where
  liftIO m = QueryT $ liftIO m

instance MonadUnliftIO m => MonadUnliftIO (QueryT m) where
  askUnliftIO =
    QueryT $
      withUnliftIO $ \u ->
       return $ UnliftIO (unliftIO u . unQueryT)

-- | Run query on a given 'Connection'. Performs no transaction
-- control.
unsafeRunQueryT :: Connection -> QueryT m a -> m a
unsafeRunQueryT c (QueryT q) = runReaderT q c

-- | Execute a query with no result.
execute :: (MonadUnliftIO m, ToRow p) => Query -> p -> QueryT m ()
execute q parameters = void $ execute' q parameters

-- | Execute a query an return the number of updated rows (if available).
execute' :: (MonadUnliftIO m, ToRow p) => Query -> p -> QueryT m Int64
execute' q parameters = QueryT $ do
  connection <- ask
  liftIO $ unsafeExecute connection q parameters

-- | Run a quest which is expected to return at most one result. Any
-- result rows past the first will be __ignored__.
query1 :: (MonadUnliftIO m, ToRow p, FromRow r) => Query -> p -> QueryT m (Maybe r)
query1 q parameters =
  query q parameters (liftIO . Streams.read)

-- | Run a query and return a list of the rows in the result. __This will read
-- ALL rows in the result into memory. It is ONLY meant for testing unless
-- you're ABSOLUTELY SURE that you won't end up using too much memory!__
queryAll :: (MonadUnliftIO m, ToRow p, FromRow r) => Query -> p -> QueryT m [r]
queryAll q parameters =
  query q parameters (liftIO . SL.toList)

-- | Execute query, returning the number of updated rows.
unsafeExecute :: (MonadUnliftIO m, ToRow p) => Connection -> Query -> p -> m Int64
unsafeExecute c q p = liftIO $ PS.execute c q p

-- | Declare a cursor.
declareCursor :: (MonadUnliftIO m) => Query -> ReaderT Connection m Cursor
declareCursor q = do
  connection <- ask
  liftIO $ PSC.declareCursor connection q

-- | Close cursor.
closeCursor :: MonadUnliftIO m => Cursor -> ReaderT Connection m ()
closeCursor cursor = liftIO $ PSC.closeCursor cursor

-- | State for query execution.
data BufferState a = Done [a]
                   | Rows [a]

-- | Stream a cursor to the given function. Does __not__ close the cursor.
streamCursor :: forall r m a . (FromRow r, MonadUnliftIO m) => Cursor -> (InputStream r -> QueryT m a) -> QueryT m a
streamCursor cursor f = QueryT $ do
    -- Start with an empty buffer.
    bufferRef <- newIORef $ Rows []
    -- Create the stream
    let !inputStream = InputStream
          (read' bufferRef)
          (unread bufferRef)
    -- Give input stream to 'f'
    unQueryT $ f inputStream

  where
    read' bufferRef =
      readIORef bufferRef >>= \case
        Done [] ->
          return Nothing
        Done (r:rs) -> do
          writeIORef bufferRef $! Done rs
          return $ Just r
        Rows [] ->
          fetch >>= \case
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

    fetch =
      reverse . either id id <$>
        PSC.foldForward cursor fetchSize (\rs r -> return (r:rs)) []

    fetchSize = 256

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result. The input stream is
-- only valid for that scope.
query :: forall p r m a . (ToRow p, FromRow r, MonadUnliftIO m) => Query -> p -> (InputStream r -> QueryT m a) -> QueryT m a
query q parameters f = QueryT $ do
  connection <- ask
  q' <- liftIO $ Query <$> PS.formatQuery connection q parameters
  bracket
    (declareCursor q')
    closeCursor
    (\c -> unQueryT $ streamCursor c f)
