{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Convertible
import qualified Data.Text as T
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable () -- PersistableValue SqlValue instance from Convertible
import Database.HDBC.Record.TH
import Database.Persist
import Database.Persist.Relational
import Database.Persist.Sql
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
import Database.Relational.Query.Pure
import Database.Relational.Query.TH (defineScalarDegree, defineScalarDegree)

data ImageType = JPEG | PNG | BMP | GIF
               deriving (Show, Eq, Enum)

-- * instances for HRR

instance Convertible ImageType Int where
    safeConvert = return . fromEnum
instance Convertible Int ImageType where
    safeConvert = return . toEnum
instance Convertible SqlValue ImageType where
    safeConvert = convertVia (undefined :: Int)
instance Convertible ImageType SqlValue where
    safeConvert = convertVia (undefined :: Int)
deriveNotNullType [t|ImageType|]
derivePersistableInstanceFromValue [t|ImageType|]
defineScalarDegree [t|ImageType|]
instance ShowConstantTermsSQL ImageType where
    showConstantTermsSQL' = showConstantTermsSQL' . fromEnum

-- * instances for persistent

instance PersistFieldSql ImageType where
    sqlType _ = SqlInt32
instance PersistField ImageType where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 v) = Right . toEnum . fromIntegral $ v
    fromPersistValue v = Left . T.pack $ "ImageType: Unkown Type, recieved: " ++ show v
defineFromToSqlPersistValue [t|ImageType|]
