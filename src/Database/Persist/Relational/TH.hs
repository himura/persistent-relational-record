{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Relational.TH
       where

import Data.Array (Array, listArray, (!))
import Data.Convertible
import Data.Int
import qualified Data.Text as T
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable () -- PersistableValue SqlValue instance from Convertible
import Database.HDBC.Record.TH
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Relational.Instances ()
import qualified Database.Persist.Sql as PersistSql
import Database.Record (PersistableWidth (..))
import Database.Record.Persistable (unsafePersistableRecordWidth)
import Database.Record.TH (deriveNotNullType, recordWidthTemplate, columnOffsetsVarNameDefault, makeRecordPersistableWithSqlTypeDefault)
import Database.Relational.Query hiding ((!))
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.TH (defineScalarDegree, defineScalarDegree, defineProductConstructorInstance, defineTableTypes)
import Language.Haskell.TH
import Language.Haskell.TH.Lib.Extra (integralE, simpleValD)
import Language.Haskell.TH.Name.CamelCase (VarName (..), toVarExp, varNameWithPrefix, varCamelcaseName)

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

defineColumnOffsets' :: Name -- ^ record type name
                     -> TypeQ -- ^ record entity type
                     -> [TypeQ] -- ^ columns
                     -> Q [Dec]
defineColumnOffsets' name entityType columns = do
    ar <- simpleValD (varName ofsVar) [t| Array Int Int |]
          [| listArray (0 :: Int, $widthIxE) $
             scanl (+) (0 :: Int) $(listE $ map recordWidthTemplate columns) |]
    pw <- [d| instance PersistableWidth $entityType where
                  persistableWidth = unsafePersistableRecordWidth $
                                     $(toVarExp ofsVar) ! $widthIxE |]
    return $ ar ++ pw
  where
    ofsVar = columnOffsetsVarNameDefault name
    widthIxE = integralE $ length columns

defineTableTypesWithConfig :: Config   -- ^ Configuration to generate query with
                           -> String   -- ^ Schema name
                           -> String   -- ^ Table name
                           -> TypeQ
                           -> [(String, TypeQ)] -- ^ Columns
                           -> Q [Dec]  -- ^ Result declarations
defineTableTypesWithConfig config schema tableName entityType columns = do
    tableDs <- defineTableTypes
               (tableVarName "tableOf")
               (relationVarName (nameConfig config) schema tableName)
               (tableVarName "insert")
               (tableVarName "insertQuery")
               entityType
               tableName
               (map fst columns)
    colsDs <- defineColumnsDefault (mkName tableName) entityType columns
    return $ tableDs ++ colsDs
  where
    tableVarName = (tableName `varNameWithPrefix`)

defineColumns :: Name               -- ^ record type name
              -> TypeQ              -- ^ entity type name
              -> [(VarName, TypeQ)] -- ^ Column info list
              -> Q [Dec]
defineColumns recName entityType cols =
    fmap concat . sequence $ zipWith defC cols [0 :: Int ..]
  where
    defC (cn, ct) ix =
        columnTemplate' entityType cn
        [| $(toVarExp . columnOffsetsVarNameDefault $ recName) ! $(integralE ix) |] ct

columnTemplate' :: TypeQ   -- ^ Record type
                -> VarName -- ^ Column declaration variable name
                -> ExpQ    -- ^ Column index expression in record (begin with 0)
                -> TypeQ   -- ^ Column type
                -> Q [Dec] -- ^ Column projection path declaration
columnTemplate' recType var' iExp colType = do
    let var = varName var'
    simpleValD var [t| Pi $recType $colType |]
        [| UnsafePi.definePi $(iExp) |]

defineColumnsDefault :: Name              -- ^ record type name
                     -> TypeQ             -- ^ entity type name
                     -> [(String, TypeQ)] -- ^ Column info list
                     -> Q [Dec]
defineColumnsDefault recName entityType cols =
    defineColumns recName entityType [(varN n, ct) | (n, ct) <- cols]
  where
    varN name = varCamelcaseName (name ++ "'")

defineHRRInstance :: Config -> String -> EntityDef -> Q [Dec]
defineHRRInstance config schema entity = do
    sqlvD  <- makeRecordPersistableWithSqlTypeDefault [t| SqlValue |] schema tableName (length columns - 1)
    tableDs <- defineTableTypesWithConfig config schema tableName entityType columns
    return $ tableDs ++ sqlvD
  where
    tableName = T.unpack . unDBName . entityDB $ entity
    recordName = T.unpack . unHaskellName . entityHaskell $ entity
    recordType = conT (mkName recordName)
    entityType = [t|Entity $recordType|]
    columns = makeColumns entity

defineTableFromPersistent :: String -> String -> [EntityDef] -> Q [Dec]
defineTableFromPersistent = defineTableFromPersistent' defaultConfig

defineTableFromPersistent' :: Config -> String -> String -> [EntityDef] -> Q [Dec]
defineTableFromPersistent' config schema tableName entities =
    case filter ((== tableName) . T.unpack . unDBName . entityDB) entities of
        (t:_) -> defineHRRInstance
                     config
                     schema
                     t
        _ -> error $ "makeColumns: Table " ++ tableName ++ " not found"

maybeNullable :: FieldDef -> Bool
maybeNullable fd = nullable (fieldAttrs fd) == Nullable ByMaybeAttr

maybeTyp :: Bool -> TypeQ -> TypeQ
maybeTyp may typ | may = conT ''Maybe `appT` typ
                 | otherwise = typ

mkHrrInstances :: [EntityDef] -> Q [Dec]
mkHrrInstances entities =
    concat `fmap` mapM mkHrrInstancesEachEntityDef entities

mkHrrInstancesEachEntityDef :: EntityDef -> Q [Dec]
mkHrrInstancesEachEntityDef entity = do
    ppkey <- mkPersistablePrimaryKey . entityId $ entity
    offs <- defineColumnOffsets' (mkName recordName) entityType colTypes
    rconD <- defineProductConstructorInstance recordType (conE . mkName $ recordName) (tail colTypes)
    return $ ppkey ++ offs ++ rconD
  where
    recordName = T.unpack . unHaskellName . entityHaskell $ entity
    recordType = conT (mkName recordName)
    entityType = [t|Entity $recordType|]
    columns = makeColumns entity
    colTypes = map snd columns

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
