{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Metadata
    ( Schema(..)
    , Table(..)
    , Typ(..)
    , toSQL
    , quoteToSQL
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

-- | A schema designation.
data Schema = DefaultSchema
            | NamedSchema Text

-- | Table name, including which schema it is in.
data Table = Table Schema Text

-- | Type name, including which schema it is in.
data Typ = Typ Schema Text

-- | Convert metadata object identifier to its quoted SQL
-- representation.
class ToSQL a where
    toSQL :: a -> Text

-- | Quote a PostgreSQL object identifier. Useful in circumstances
-- where you need to quote a dynamically generated identifier.
quoteToSQL :: Text -> Text
quoteToSQL i = T.concat [ singleQt
                        , T.replace singleQt doubleQt i
                        , singleQt
                        ]
  where
    singleQt = "\""
    doubleQt = "\"\""

--
-- Instances
--

instance ToSQL Schema where
  toSQL DefaultSchema = quoteToSQL "public"
  toSQL (NamedSchema schemaId) = quoteToSQL schemaId

instance ToSQL Table where
  toSQL (Table schema tableId) =
    T.concat [toSQL schema, ".", quoteToSQL tableId]

instance ToSQL Typ where
  toSQL (Typ schema tableId) =
    T.concat [toSQL schema, ".", quoteToSQL tableId]

