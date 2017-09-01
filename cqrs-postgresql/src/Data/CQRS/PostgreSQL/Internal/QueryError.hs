{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.QueryError
       ( QueryError(..)
       , isDuplicateKey
       ) where

import           Control.Exception (Exception)
import           Data.ByteString (ByteString)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

-- | Is the given query exception a duplicate key exception?
isDuplicateKey :: QueryError -> Maybe ()
isDuplicateKey qe | qeSqlState qe == Just "23505" = Just ()
                  | otherwise                     = Nothing

-- | Error happened during query.
data QueryError = QueryError
    { qeSqlState :: Maybe ByteString
    , qeStatusMessage :: ByteString
    , qeErrorMessage :: Maybe ByteString
    } deriving (Show, Typeable, Generic)

instance Exception QueryError
