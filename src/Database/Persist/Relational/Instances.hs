{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Relational.Instances
       where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Database.Persist
import Database.Persist.Relational.TH
import Database.Record.FromSql
import Database.Record.ToSql
import Database.Relational.Query (ProductConstructor (..))

instance PersistEntity record
         => ProductConstructor (Key record -> record -> Entity record) where
    productConstructor = Entity

instance (ToSql PersistValue (Key record), ToSql PersistValue record)
         => ToSql PersistValue (Entity record) where
    recordToSql = wrapToSql $ \(Entity k v) -> do
        putRecord k
        putRecord v

instance (PersistEntity record, FromSql PersistValue (Key record), FromSql PersistValue record)
         => FromSql PersistValue (Entity record) where
    recordFromSql = Entity <$> recordFromSql <*> recordFromSql

derivePersistableInstancesFromPersistFieldInstances ["SomePersistField"]
