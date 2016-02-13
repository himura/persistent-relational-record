{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Relational.Instances
       where

import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Word
import Database.Persist
import Database.Persist.Relational.TH
import Database.Record.FromSql
import Database.Record.Persistable
import Database.Record.ToSql
import Database.Relational.Query (ProductConstructor (..))
import Numeric.Natural

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

defineFromToSqlPersistValue [t|Bool|]
defineFromToSqlPersistValue [t|Double|]
defineFromToSqlPersistValue [t|Int|]
defineFromToSqlPersistValue [t|Int8|]
defineFromToSqlPersistValue [t|Int16|]
defineFromToSqlPersistValue [t|Int32|]
defineFromToSqlPersistValue [t|Int64|]
defineFromToSqlPersistValue [t|Rational|]
defineFromToSqlPersistValue [t|Word|]
defineFromToSqlPersistValue [t|Word8|]
defineFromToSqlPersistValue [t|Word16|]
defineFromToSqlPersistValue [t|Word32|]
defineFromToSqlPersistValue [t|Word64|]
defineFromToSqlPersistValue [t|String|]
defineFromToSqlPersistValue [t|ByteString|]
defineFromToSqlPersistValue [t|T.Text|]
defineFromToSqlPersistValue [t|TL.Text|]
defineFromToSqlPersistValue [t|UTCTime|]
defineFromToSqlPersistValue [t|Natural|]
-- defineFromToSqlPersistValue [t|Html|]
defineFromToSqlPersistValue [t|TimeOfDay|]
defineFromToSqlPersistValue [t|Day|]
defineFromToSqlPersistValue [t|PersistValue|]
defineFromToSqlPersistValue [t|Checkmark|]

-- overlap instances...
-- instance PersistField a => ToSql PersistValue a where
--     recordToSql = valueToSql

-- instance PersistField a => FromSql PersistValue a where
--     recordFromSql = valueFromSql
