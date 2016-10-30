{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
--
-- See: <https://github.com/himura/persistent-relational-record#readme>
module Database.Persist.Relational
       ( runQuery
       , rawQuery
       , mkHrrInstances
       , defineTableFromPersistent
       , defineTableFromPersistentWithConfig
       , defineFromToSqlPersistValue
       , defaultConfig
       , ToPersistEntity (..)
       ) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
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

-- | Execute a HRR 'Query' and return the stream of its results.
runQuery :: ( MonadResource m
            , MonadReader env m
#if MIN_VERSION_persistent(2, 5, 0)
            , HasPersistBackend env
            , BaseBackend env ~ SqlBackend
#else
            , HasPersistBackend env SqlBackend
#endif
            , ToSql PersistValue p
            , ToPersistEntity a b
            )
         => Query p a -- ^ Query to get record type a requires parameter p
         -> p         -- ^ Parameter type
         -> Source m b
runQuery q vals = rawQuery q vals $= CL.map (runToRecord recordFromSql')

runQueryList
    :: ( MonadResourceBase m
       , MonadReader backend m
#if MIN_VERSION_persistent(2, 5, 0)
       , HasPersistBackend backend
       , BaseBackend backend ~ SqlBackend
#else
       , HasPersistBackend backend SqlBackend
#endif
       , ToSql PersistValue p
       , ToPersistEntity a b
       )
    => Query p a -> p -> ReaderT backend m [b]
runQueryList q vals = runResourceT $ runQuery q vals $$ CL.consume

rawQuery :: ( MonadResource m
            , MonadReader env m
#if MIN_VERSION_persistent(2, 5, 0)
            , HasPersistBackend env
            , BaseBackend env ~ SqlBackend
#else
            , HasPersistBackend env SqlBackend
#endif
            , ToSql PersistValue p
            )
         => Query p a
         -> p
         -> Source m [PersistValue]
rawQuery q vals = PersistSql.rawQuery queryTxt params
  where
    queryTxt = T.pack . untypeQuery $ q
    params = runFromRecord recordToSql vals
