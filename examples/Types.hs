{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Relational
import Database.Persist.Sql
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
import Database.Relational
import Database.Relational.TH (defineScalarDegree, defineScalarDegree)

data UserStatus
    = UserActive
    | UserSuspended
    | UserWithdrawn
    deriving (Show, Eq, Enum)

-- * instances for HRR

deriveNotNullType [t|UserStatus|]
defineFromToSqlPersistValue [t|UserStatus|]
defineScalarDegree [t|UserStatus|]
instance LiteralSQL UserStatus where
    showLiteral' = showLiteral' . fromEnum

-- * instances for persistent

instance PersistFieldSql UserStatus where
    sqlType _ = SqlInt32
instance PersistField UserStatus where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 v) = Right . toEnum . fromIntegral $ v
    fromPersistValue v = Left . T.pack $ "UserStatus: Unknown Type, received: " ++ show v
