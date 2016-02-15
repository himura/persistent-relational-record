{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Relational.Instances
       where

import Database.HDBC (SqlValue)
import Database.Persist
import Database.Record.FromSql
import Database.Record.ToSql
import Database.Relational.Query (ProductConstructor (..))

instance PersistEntity record
         => ProductConstructor (Key record -> record -> Entity record) where
    productConstructor = Entity

instance (ToSql SqlValue (Key record), ToSql SqlValue record)
         => ToSql SqlValue (Entity record) where
    recordToSql = wrapToSql $ \(Entity k v) -> do
        putRecord k
        putRecord v

instance (PersistEntity record, FromSql SqlValue (Key record), FromSql SqlValue record)
         => FromSql SqlValue (Entity record) where
    recordFromSql = Entity <$> recordFromSql <*> recordFromSql
