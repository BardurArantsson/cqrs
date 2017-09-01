{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Identifiers
    ( Identifiers(..)
    , mkIdentifiers
    ) where

import           Database.Peregrin.Metadata

-- | Core tables.
data Identifiers = Identifiers { tblEvent :: QIdentifier
                               , tblSnapshot :: QIdentifier
                               }

-- | Create core tables for a given schema.
mkIdentifiers :: Schema -> Identifiers
mkIdentifiers schema = Identifiers { tblEvent = QIdentifier schema "event"
                                   , tblSnapshot = QIdentifier schema "snapshot"
                                   }
