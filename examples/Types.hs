{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational
import Database.Persist.Sql
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
import Database.Relational.Query.Pure
import Database.Relational.Query.TH (defineScalarDegree, defineScalarDegree)

data ImageType = JPEG | PNG | BMP | GIF
               deriving (Show, Eq, Enum)

-- * instances for HRR

deriveNotNullType [t|ImageType|]
defineFromToSqlPersistValue [t|ImageType|]
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
