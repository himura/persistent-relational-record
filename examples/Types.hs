{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational
import Database.Persist.Sql
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
#if MIN_VERSION_relational_query(0, 10, 0)
import Database.Relational
import Database.Relational.TH (defineScalarDegree, defineScalarDegree)
#else
import Database.Relational.Query
import Database.Relational.Query.TH (defineScalarDegree, defineScalarDegree)
#endif

data ImageType = JPEG | PNG | BMP | GIF
               deriving (Show, Eq, Enum)

-- * instances for HRR

deriveNotNullType [t|ImageType|]
defineFromToSqlPersistValue [t|ImageType|]
defineScalarDegree [t|ImageType|]
instance LiteralSQL ImageType where
    showLiteral' = showLiteral' . fromEnum

-- * instances for persistent

instance PersistFieldSql ImageType where
    sqlType _ = SqlInt32
instance PersistField ImageType where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 v) = Right . toEnum . fromIntegral $ v
    fromPersistValue v = Left . T.pack $ "ImageType: Unkown Type, recieved: " ++ show v
