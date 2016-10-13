module Data.CQRS.Types.Snapshot
       ( Snapshot(..)
       ) where

import Data.Int (Int32)

-- | Snapshot of a value at some particular version.
data Snapshot a =
  Snapshot { sVersion :: {-# UNPACK #-} !Int32
           , sSnapshot :: !a
           } deriving (Eq, Ord, Show)
