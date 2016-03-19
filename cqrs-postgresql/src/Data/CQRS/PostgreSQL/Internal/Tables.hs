{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Tables
    ( mkTables
    , Tables(..)
    ) where

import           Data.CQRS.PostgreSQL.Metadata
import           Data.Text (Text)

data Tables = Tables { tblEvent :: Text
                     , tblSnapshot :: Text
                     }

mkTables :: Schema -> Tables
mkTables schema = Tables { tblEvent = toSQL $ Table schema "event"
                         , tblSnapshot = toSQL $ Table schema "snapshot"
                         }
