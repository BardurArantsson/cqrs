module Data.CQRS.Repository
    ( -- * Repository
      Repository
    , newRepository
      -- * Settings
    , Settings
    , setSnapshotFrequency
    , defaultSettings
    ) where

import Data.CQRS.Internal.Repository
