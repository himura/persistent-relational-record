{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Relational.TH
       where

import Data.Convertible
import Data.Int
import qualified Data.Text as T
import Database.HDBC (SqlValue)
import Database.HDBC.Query.TH
import Database.HDBC.Record.TH
import Database.Persist
import Database.Persist.Quasi
import qualified Database.Persist.Sql as PersistSql
import Database.Record.TH (deriveNotNullType, deriveNotNullType)
import Database.Relational.Query
import Database.Relational.Query.TH (defineScalarDegree)
import Language.Haskell.TH

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
defineTableFromPersistent = defineTableFromPersistent' defaultConfig

defineTableFromPersistent' :: Config -> String -> String -> [EntityDef] -> Q [Dec]
defineTableFromPersistent' config schema tableName entities =
    case filter ((== tableName) . T.unpack . unDBName . entityDB) entities of
        (t:_) -> defineTableDefault
                     config
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
    concat `fmap` mapM (mkPersistablePrimaryKey . entityId) entities

mkPersistablePrimaryKey :: FieldDef -> Q [Dec]
mkPersistablePrimaryKey fd = do
    convertibleD <- mkConvertible typ
    notNullD <- deriveNotNullType typ
    persistableD <- derivePersistableInstanceFromValue typ
    scalarDegD <- defineScalarDegree typ
    showCTermSQLD <- mkShowConstantTermsSQL typ
    return $ convertibleD ++ notNullD ++ persistableD ++ scalarDegD ++ showCTermSQLD
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

mkShowConstantTermsSQL :: TypeQ -> Q [Dec]
mkShowConstantTermsSQL typ =
    [d|instance ShowConstantTermsSQL $typ where
           showConstantTermsSQL' = showConstantTermsSQL' . PersistSql.fromSqlKey|]

mkFieldType :: FieldDef -> TypeQ
mkFieldType fd = maybeTyp (maybeNullable fd) (ftToType . fieldType $ fd)
