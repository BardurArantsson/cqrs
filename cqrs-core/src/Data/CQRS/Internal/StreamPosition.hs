{-# LANGUAGE DeriveGeneric #-}
module Data.CQRS.Internal.StreamPosition
       ( StreamPosition(..)
       , streamPositionToBytes
       , bytesToStreamPosition
       ) where

import           Control.DeepSeq (NFData)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Lex.Integral (readDecimal, packDecimal)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

-- | A stream position is an implementation-defined
-- representation of the position in an 'EventStream'.
newtype StreamPosition = StreamPosition { unStreamPosition :: Int64 }
    deriving (Eq, Ord, Show, Generic, Typeable)

instance NFData StreamPosition

-- | Convert a 'StreamPosition' to a 'ByteString'. The format of
-- the underlying binary data __MUST__ be treated as completely
-- opaque, i.e. it can only be saved/restored from external storage
-- and no other type of manipulation of the data is defined.
streamPositionToBytes :: MonadIO m => StreamPosition -> m ByteString
streamPositionToBytes (StreamPosition i) =
    case packDecimal i of
      Just s -> return $ s
      Nothing -> error $ "positionToBytes: position was negative: " ++ show i

-- | Convert a 'ByteString' to a 'StreamPosition'. The 'ByteString'
-- __MUST__ ultimately have been obtained through a call to
-- 'streamPositionToBytes'. All kinds of nasty behavior may ensue if
-- that is not the case.
bytesToStreamPosition :: MonadIO m => ByteString -> m (Either String StreamPosition)
bytesToStreamPosition s = return $
    case readDecimal s of
      Just (i, t) | B.null t -> Right $ StreamPosition i
      Just _                 -> Left "Invalid position string"
      Nothing                -> Left "Invalid position string"
