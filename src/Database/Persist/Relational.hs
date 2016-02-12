{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Relational
       ( runQuery
       , rawQuery
       , mkHrrInstances
       , defineTableFromPersistent
       , defineTableFromPersistent'
       ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Source, ($=))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Database.HDBC (SqlValue)
import qualified Database.HDBC as HDBC
import Database.Persist
import Database.Persist.Relational.TH
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as PersistSql
import Database.Record (FromSql, ToSql, recordFromSql, recordToSql, runFromRecord, runToRecord)
import Database.Relational.Query

runQuery :: ( MonadResource m
            , MonadReader env m
            , HasPersistBackend env SqlBackend
            , ToSql PersistValue p
            , FromSql SqlValue a
            )
         => Query p a
         -> p
         -> Source m a
runQuery q vals = rawQuery q vals $= CL.map (runToRecord recordFromSql . map pToSql)

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

pToSql :: PersistValue -> HDBC.SqlValue
pToSql (PersistText s) = HDBC.SqlString (T.unpack s)
pToSql (PersistByteString bs) = HDBC.SqlByteString bs
pToSql (PersistInt64 i) = HDBC.SqlInt64 i
pToSql (PersistDouble d) = HDBC.SqlDouble d
pToSql (PersistBool b) = HDBC.SqlBool b
pToSql (PersistDay d) = HDBC.SqlLocalDate d
pToSql (PersistTimeOfDay t) = HDBC.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = HDBC.SqlUTCTime t
pToSql (PersistRational t) = HDBC.SqlRational t
pToSql (PersistDbSpecific t) = HDBC.SqlByteString t
pToSql PersistNull = HDBC.SqlNull
pToSql (PersistList _) = error "not implemented"
pToSql (PersistMap _) = error "not implemented"
pToSql (PersistObjectId _) = error "Do not support MongoDB backend"
