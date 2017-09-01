module Data.CQRS.Repository
    ( -- * Repository
      Repository
    , newRepository
      -- * Settings
    , Settings
    , setSnapshotFrequency
    , setClock
    , defaultSettings
    ) where

import Data.CQRS.Internal.Repository
