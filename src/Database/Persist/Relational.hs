{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational.Instances ()
import Database.Persist.Relational.TH
import Database.Persist.Relational.ToPersistEntity
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as PersistSql
import Database.Record.ToSql (ToSql, recordToSql, runFromRecord)
import Database.Record.FromSql (runToRecord)
import Database.Relational

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
         -> ConduitT () b m ()
runQuery q vals = rawQuery q vals .| CL.map (runToRecord recordFromSql')

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
         -> ConduitT () [PersistValue] m ()
rawQuery q vals = PersistSql.rawQuery queryTxt params
  where
    queryTxt = T.pack . untypeQuery $ q
    params = runFromRecord recordToSql vals
