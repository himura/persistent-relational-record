{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Relational
       where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Source)
import qualified Data.Text as T
import Database.Persist.Sql (PersistValue, SqlBackend, HasPersistBackend, rawQuery)
import Database.Record.ToSql (ToSql, recordToSql, runFromRecord)
import Database.Relational.Query (Query, untypeQuery)

runQuery :: ( MonadResource m
            , MonadReader env m
            , HasPersistBackend env SqlBackend
            , ToSql PersistValue p
            )
         => Query p a
         -> p
         -> Source m [PersistValue]
runQuery query vals = rawQuery queryTxt params
  where
    queryTxt = T.pack . untypeQuery $ query
    params = runFromRecord recordToSql vals

