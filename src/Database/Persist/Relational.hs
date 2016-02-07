{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Relational
       where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Source, ($=))
import qualified Data.Conduit.List as CL
import Data.Convertible
import Data.Int
import qualified Data.Text as T
import Database.HDBC (SqlValue)
import qualified Database.HDBC as HDBC
import Database.HDBC.Query.TH
import Database.HDBC.Record.Persistable () -- PersistableValue SqlValue instance from Convertible
import Database.HDBC.Record.TH
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as PersistSql
import Database.Record (FromSql, ToSql, recordFromSql, recordToSql, runFromRecord, runToRecord)
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
import Database.Relational.Query
import Database.Relational.Query.TH (defineScalarDegree, defineScalarDegree)
import Language.Haskell.TH

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

-- parseEntity :: PersistEntity record => [PersistValue] -> record
-- parseEntity vals = case fromPersistValues vals of
--     Left e -> error $ T.unpack e
--     Right v -> v

ftToType :: FieldType -> TypeQ
ftToType (FTTypeCon Nothing t) = conT $ mkName $ T.unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
ftToType (FTTypeCon (Just m) t) = conT $ mkName $ T.unpack $ T.concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `appT` ftToType y
ftToType (FTList x) = listT `appT` ftToType x

makeColumns :: EntityDef
            -> [(String, TypeQ)]
makeColumns t =
    mkCol (entityId t) : map mkCol (entityFields t)
  where
    mkCol fd = (toS $ fieldDB fd, mkFieldType fd)
    toS = T.unpack . unDBName

defineTableFromPersistent :: String -> String -> [EntityDef] -> Q [Dec]
defineTableFromPersistent schema tableName entities =
    case filter ((== tableName) . T.unpack . unDBName . entityDB) entities of
        (t:_) -> defineTableDefault
                     defaultConfig
                     schema
                     tableName
                     (makeColumns t)
                     (map (mkName . T.unpack) . entityDerives $ t)
                     [0]
                     (Just 0)
        _ -> error $ "makeColumns: Table " ++ tableName ++ " not found"

maybeNullable :: FieldDef -> Bool
maybeNullable fd = nullable (fieldAttrs fd) == Nullable ByMaybeAttr

maybeTyp :: Bool -> TypeQ -> TypeQ
maybeTyp may typ | may = conT ''Maybe `appT` typ
                 | otherwise = typ

mkHrrInstances :: [EntityDef] -> Q [Dec]
mkHrrInstances entities =
    concat <$> mapM (mkPersistablePrimaryKey . entityId) entities

mkPersistablePrimaryKey :: FieldDef -> Q [Dec]
mkPersistablePrimaryKey fd = do
    convertibleD <- mkConvertible typ
    notNullD <- deriveNotNullType typ
    persistableD <- derivePersistableInstanceFromValue typ
    scalarDegD <- defineScalarDegree typ
    return $ convertibleD ++ notNullD ++ persistableD ++ scalarDegD
  where
    typ = mkFieldType fd

mkConvertible :: TypeQ -> Q [Dec]
mkConvertible typ = do
    svToId <-
        [d|instance Convertible SqlValue $typ where
               safeConvert = convertVia (undefined :: Int64)|]
    idToSv <-
        [d|instance Convertible $typ SqlValue where
               safeConvert = convertVia (undefined :: Int64)|]
    intToId <-
        [d|instance Convertible Int64 $typ where
               safeConvert = return . PersistSql.toSqlKey|]
    idToInt <-
        [d|instance Convertible $typ Int64 where
               safeConvert = return . PersistSql.fromSqlKey|]
    return $ svToId ++ idToSv ++ intToId ++ idToInt

mkFieldType :: FieldDef -> TypeQ
mkFieldType fd = maybeTyp (maybeNullable fd) (ftToType . fieldType $ fd)

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
