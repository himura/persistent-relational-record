{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Relational.Instances
       where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational.TH
import Database.Record.FromSql
import Database.Record.Persistable
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

instance PersistableType PersistValue where
    persistableType = unsafePersistableSqlTypeFromNull PersistNull

instance PersistField a => PersistableValue PersistValue a where
    persistableValue = persistableSqlValue persistableType unsafeFromPersistValue toPersistValue
      where
        unsafeFromPersistValue v =
            case fromPersistValue v of
                Left err -> error $ T.unpack err
                Right a -> a

do
    types <- persistValueTypesFromPersistFieldInstances ["SomePersistField"]
    concat `fmap` mapM defineFromToSqlPersistValue types
