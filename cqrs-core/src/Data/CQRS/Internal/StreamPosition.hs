{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.Internal.StreamPosition
       ( StreamPosition(..)
       , streamPositionToBytes
       , bytesToStreamPosition
       , infimum
       ) where

import           Control.DeepSeq (NFData)
import           Data.ByteString.Lex.Integral (readDecimal, packDecimal)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

-- | The position before the first StreamPosition. All other
-- 'StreamPosition' values compare as greater than this value.
infimum :: StreamPosition
infimum = StreamPosition (-1)

-- | A stream position is an implementation-defined
-- representation of the position in an 'EventStream'.
newtype StreamPosition = StreamPosition { unStreamPosition :: Int64 }
    deriving (Eq, Ord, Show, Generic, Typeable)

instance NFData StreamPosition

-- | Convert a 'StreamPosition' to a 'ByteString'. The format of
-- the underlying binary data __MUST__ be treated as completely
-- opaque, i.e. it can only be saved/restored from external storage
-- and no other type of manipulation of the data is defined.
streamPositionToBytes :: StreamPosition -> ByteString
streamPositionToBytes sp@(StreamPosition i) = B.concat
  [ "0;" -- Version number
  , if sp == infimum then
      B.empty
    else
      fromMaybe
        (error $ "positionToBytes: position was negative: " ++ show i)
        (packDecimal i)
  ]

-- | Convert a 'ByteString' to a 'StreamPosition'. The 'ByteString'
-- __MUST__ ultimately have been obtained through a call to
-- 'streamPositionToBytes'. All kinds of nasty behavior may ensue if
-- that is not the case.
bytesToStreamPosition :: ByteString -> Either String StreamPosition
bytesToStreamPosition s =
  -- Only need to handle version 0, so just hardcode.
  case B.stripPrefix "0;" s of
    Nothing -> Left "Bad version"
    Just s' ->
      if B.null s' then
        pure infimum
      else
        case readDecimal s' of
          Just (i, t) | B.null t -> Right $ StreamPosition i
          Just _                 -> Left "Invalid position string"
          Nothing                -> Left "Invalid position string"
