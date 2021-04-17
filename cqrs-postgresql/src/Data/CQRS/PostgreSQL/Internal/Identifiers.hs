{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Identifiers
    ( Identifiers(..)
    , mkIdentifiers
    ) where

import           Database.Peregrin.Metadata

-- | Core tables.
data Identifiers = Identifiers { tblEvent :: QIdentifier
                               , tblKvStore :: QIdentifier
                               , tblSnapshot :: QIdentifier
                               }

-- | Create core tables for a given schema.
mkIdentifiers :: Schema -> Identifiers
mkIdentifiers schema = Identifiers { tblEvent = QIdentifier schema "event"
                                   , tblKvStore = QIdentifier schema "kvstore"
                                   , tblSnapshot = QIdentifier schema "snapshot"
                                   }
