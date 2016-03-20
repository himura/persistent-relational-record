{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Relational.ToPersistEntity
       where

import Database.Record.FromSql (RecordFromSql, recordFromSql, (<&>))
import Database.Persist (PersistValue)

class ToPersistEntity a b | a -> b, b -> a where
    recordFromSql' :: RecordFromSql PersistValue b

instance ToPersistEntity () () where
    recordFromSql' = recordFromSql

instance (ToPersistEntity a c, ToPersistEntity b d) =>  ToPersistEntity (a, b) (c, d) where
    recordFromSql' = recordFromSql' <&> recordFromSql'
