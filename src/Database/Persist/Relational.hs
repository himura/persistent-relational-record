{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Relational
       ( runQuery
       , rawQuery
       , mkHrrInstances
       , defineTableFromPersistent
       , defineFromToSqlPersistValue
       , defaultConfig
       ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Source, ($=))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational.Instances ()
import Database.Persist.Relational.TH
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as PersistSql
import Database.Record (FromSql, ToSql, recordFromSql, recordToSql, runFromRecord, runToRecord)
import Database.Relational.Query

runQuery :: ( MonadResource m
            , MonadReader env m
            , HasPersistBackend env SqlBackend
            , ToSql PersistValue p
            , FromSql PersistValue a
            )
         => Query p a
         -> p
         -> Source m a
runQuery q vals = rawQuery q vals $= CL.map (runToRecord recordFromSql)

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
