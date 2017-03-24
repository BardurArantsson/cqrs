{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Tables
    ( Tables(..)
    , mkTables
    ) where

import           Data.CQRS.PostgreSQL.Metadata
import           Data.Text (Text)

-- | Core tables.
data Tables = Tables { tblEvent :: Text
                     , tblSnapshot :: Text
                     }

-- | Create core tables for a given schema.
mkTables :: Schema -> Tables
mkTables schema = Tables { tblEvent = toSQL $ Table schema "event"
                         , tblSnapshot = toSQL $ Table schema "snapshot"
                         }
