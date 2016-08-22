{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Database.Persist.Relational
-- Copyright   :  (C) 2016 Takahiro Himura
-- License     :  BSD3
-- Maintainer  :  Takahiro Himura <taka@himura.jp>
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module works as a bridge between <https://hackage.haskell.org/package/relational-query Haskell Relational Record>
-- and <http://hackage.haskell.org/package/persistent Persistent>.
-- It uses the persistent entities definition instead of obtaining schema from DB at compilation time.
module Database.Persist.Relational
       ( -- * Getting Started
         -- $GettingStarted
         runQuery
       , rawQuery
       , mkHrrInstances
       , defineTableFromPersistent
       , defineTableFromPersistentWithConfig
       , defineFromToSqlPersistValue
       , defaultConfig
       , ToPersistEntity (..)
       ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Source, ($=))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational.Instances ()
import Database.Persist.Relational.TH
import Database.Persist.Relational.ToPersistEntity
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as PersistSql
import Database.Record (ToSql, recordToSql, runFromRecord, runToRecord)
import Database.Relational.Query

-- $GettingStarted
--
-- If you already define an entities in persistent's manner, then you are almost ready to use this module.
-- The entities definition in the style of persistent-relational-record are shown below:
--
-- @
-- share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "db", mkHrrInstances] [persistLowerCase|
-- Post
--     title      Text
--     deriving Eq Show
-- Tag
--     name       Text
--     deriving Eq Show
-- PostTag
--     postId     PostId
--     tagId      TagId
-- |]
-- @
--
-- The main difference is that @mkSave "db"@ and @mkHrrInstances@ has been added to the 1st argument of the @share@ function.

-- | Execute a HRR 'Query' and return the stream of its results.
runQuery :: ( MonadResource m
            , MonadReader env m
            , HasPersistBackend env SqlBackend
            , ToSql PersistValue p
            , ToPersistEntity a b
            )
         => Query p a -- ^ Query to get record type a requires parameter p
         -> p         -- ^ Parameter type
         -> Source m b
runQuery q vals = rawQuery q vals $= CL.map (runToRecord recordFromSql')

rawQuery :: ( MonadResource m
            , MonadReader env m
            , HasPersistBackend env SqlBackend
            , ToSql PersistValue p
            )
         => Query p a
         -> p
         -> Source m [PersistValue]
rawQuery q vals = PersistSql.rawQuery queryTxt params
  where
    queryTxt = T.pack . untypeQuery $ q
    params = runFromRecord recordToSql vals
