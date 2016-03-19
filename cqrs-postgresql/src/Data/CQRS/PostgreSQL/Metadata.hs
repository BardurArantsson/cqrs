{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Metadata
    ( Schema(..)
    , Table(..)
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

-- | Turn a 'Table' into a fully-qualified SQL string.
toSQL :: Table -> Text
toSQL (Table DefaultSchema tableId) =
    quoteToSQL tableId
toSQL (Table (NamedSchema schemaId) tableId) =
    T.concat [quoteToSQL schemaId, ".", quoteToSQL tableId]
