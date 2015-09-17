module Data.CQRS.Types.Snapshot
       ( Snapshot(..)
       ) where

-- | Snapshot of a value at some particular version.
data Snapshot a =
  Snapshot { sVersion :: {-# UNPACK #-} !Int
           , sSnapshot :: !a
           } deriving (Eq, Ord, Show)
