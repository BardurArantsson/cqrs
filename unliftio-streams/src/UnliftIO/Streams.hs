module UnliftIO.Streams
  ( -- * Stream types
    InputStream

    -- * Primitive stream operations
  , read

    -- * Batteries included
  , module UnliftIO.Streams.Combinators
  , module UnliftIO.Streams.List
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Prelude hiding (read)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as S
import           UnliftIO.Streams.Combinators
import           UnliftIO.Streams.List

{-# INLINE read #-}
read :: MonadUnliftIO m => InputStream a -> m (Maybe a)
read is = liftIO $ S.read is
