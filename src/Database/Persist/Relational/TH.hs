{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Persist.Relational.TH where

import qualified Data.Array as Arr
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Relational.Config
import qualified Database.Persist.Sql as PersistSql
import Database.Record (PersistableWidth (..))
import Database.Record.FromSql (FromSql(..), valueRecordFromSql)
import Database.Record.Persistable
    ( PersistableRecordWidth
    , ProductConst
    , genericFieldOffsets
    , getProductConst
    , runPersistableRecordWidth
    )
import Database.Record.TH (deriveNotNullType)
import Database.Record.ToSql (ToSql (..), valueRecordToSql)
import Database.Relational (LiteralSQL(..))
import Database.Relational.OverloadedProjection (HasProjection(..))
import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.TH (defineTableTypes, defineScalarDegree)
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Name.CamelCase (VarName (..), varCamelcaseName)

mkHrr :: [EntityDef] -> Q [Dec]
mkHrr = mkHrrWithConfig defaultNameConfig

mkHrrWithConfig :: NameConfig -> [EntityDef] -> Q [Dec]
mkHrrWithConfig conf = concatMapM (mkHrrForEntityDef conf)

mkHrrForEntityDef :: NameConfig -> EntityDef -> Q [Dec]
mkHrrForEntityDef conf ent = do
    pkeyInstances <- mkPersistablePrimaryKey idType
    tableTypes <- defineTableTypesForTableDef conf tableDef
    piProjections <- defineMonomorphicProjections tableDef
    pwidthInstances <- definePersistableWidthInstances tableType
    fromSqlInstances <- defineFromSqlPersistValueInstances tableType

    return $ pkeyInstances ++ tableTypes ++ piProjections ++ pwidthInstances ++ fromSqlInstances
  where
    tableDef@TableDef { tableType, tableIdColumn = Column {columnType = idType} } = entityDefToTableDef conf ent

-- | standalone deriving Generic instance for entity IDs.
--
-- Persistent does not derive Generic instances for `Key a`.
-- see https://github.com/yesodweb/persistent/pull/734#issuecomment-346696921
--     https://github.com/yesodweb/persistent/issues/578
deriveGenericForEntityId :: [EntityDef] -> Q [Dec]
deriveGenericForEntityId entityDefs =
    concatMapM mkDerivingGeneric $ map (mkFieldType . entityId) entityDefs
  where
    mkDerivingGeneric typ = [d| deriving instance Generic $typ |]

data TableDef = TableDef
    { tableType :: TypeQ
    , tableTypeName :: String
    , tableDatabaseName :: String
    , tableIdColumn :: Column
    , tableColumns :: [Column]
    }

data Column = Column
    { columnDBName :: String
    , columnLabelName :: String
    , columnType :: TypeQ
    }

entityDefToTableDef :: NameConfig -> EntityDef -> TableDef
entityDefToTableDef conf@NameConfig {..} ent =
    TableDef
    { tableType = conT $ mkName typeName
    , tableTypeName = typeName
    , tableDatabaseName = T.unpack . unDBName . entityDB $ ent
    , tableIdColumn = makeColumn conf $ entityId ent
    , tableColumns = map (makeColumn conf) $ entityFields ent
    }
  where
    typeName = T.unpack . unHaskellName . entityHaskell $ ent

ftToType :: FieldType -> TypeQ
ftToType (FTTypeCon Nothing t) = conT $ mkName $ T.unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
ftToType (FTTypeCon (Just m) t) = conT $ mkName $ T.unpack $ T.concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `appT` ftToType y
ftToType (FTList x) = listT `appT` ftToType x

makeColumn :: NameConfig -> FieldDef -> Column
makeColumn NameConfig{columnNameToLabelName} fd@FieldDef { fieldDB = DBName dbName } =
    Column
    { columnDBName = columnName
    , columnLabelName = columnNameToLabelName columnName
    , columnType = mkFieldType fd
    }
  where
    columnName = T.unpack dbName

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM = (fmap concat .) . mapM

-- | Generate `HasProjection "columnName" (Entity table) columnType` instances
defineMonomorphicProjections :: TableDef -> Q [Dec]
defineMonomorphicProjections TableDef{tableTypeName = tableTypeNameStr, tableIdColumn, tableColumns} = do
    columnOffsetsVarName <- newName $ "columnOffsets" ++ tableTypeNameStr
    columnOffsetsEntityVarName <- newName $ "columnOffsetsEntity" ++ tableTypeNameStr
    coVar <- defineColumnOffsetsVar tableTypeName columnOffsetsVarName columnOffsetsEntityVarName
    piPrjs <- concatMapM (definePiProjection tableTypeName columnOffsetsEntityVarName) $ zip [0..] columns
    return $ coVar ++ piPrjs
  where
    tableTypeName = mkName tableTypeNameStr
    columns = tableIdColumn : tableColumns

-- | Generate a `HasProjection "columnName" (Entity table) columnType` instance
definePiProjection ::
       Name -- ^ table type name
    -> Name -- ^ columnOffsetsVarName
    -> (Integer, Column) -- ^ (column idx, (column name, column type))
    -> Q [Dec]
definePiProjection tableTypeName columnOffsetsEntityVarName (colIdx, Column {..}) =
    [d| instance HasProjection $(litT . strTyLit $ columnLabelName) (Entity $(conT tableTypeName)) $(columnType) where
            projection _ = definePi $ $(varE columnOffsetsEntityVarName) Arr.! $(litE . integerL $ colIdx)
    |]

defineColumnOffsetsVar ::
       Name -- ^ table type name
    -> Name -- ^ columnOffsetsVarName
    -> Name -- ^ columnOffsetsEntityVarName
    -> Q [Dec]
defineColumnOffsetsVar tableTypeName columnOffsetsVarName columnOffsetsEntityVarName = do
    sig <- sigD columnOffsetsVarName [t| Arr.Array Int Int |]
    val <- valD (varP columnOffsetsVarName) (normalB [| getProductConst (genericFieldOffsets :: ProductConst (Arr.Array Int Int) $(conT tableTypeName)) |]) []

    -- `Entity val` has nested data structure, but we assume that it has flat data structure like: (id, val1, val2, ...)
    sigEnt <- sigD columnOffsetsEntityVarName [t| Arr.Array Int Int |]
    valEnt <-
        valD
            (varP columnOffsetsEntityVarName)
            (normalB
                 [|Arr.listArray (0, length $(varE columnOffsetsVarName)) $
                     (0 :) . map (+ runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth (PersistSql.Key $(conT tableTypeName)))) . Arr.elems $ $(varE columnOffsetsVarName)
                  |])
            []
    return [sig, val, sigEnt, valEnt]

defineTableTypesForTableDef :: NameConfig -> TableDef -> Q [Dec]
defineTableTypesForTableDef conf TableDef {..} =
    defineTableTypes
        tableOfVarName
        tableVarName
        insertVarName
        insertQueryVarName
        [t|Entity $tableType|]
        tableDatabaseName
        colNames
  where
    tableOfVarName = varCamelcaseName $ "tableOf_" ++ tableTypeName
    mkVarName f = VarName $ mkName $ f conf tableTypeName
    tableVarName = mkVarName tableVarNameFromTableTypeName
    insertVarName = mkVarName insertVarNameFromTableTypeName
    insertQueryVarName = mkVarName insertQueryVarNameFromTableTypeName
    colNames = map columnDBName $ tableIdColumn : tableColumns


definePersistableWidthInstances :: TypeQ -> Q [Dec]
definePersistableWidthInstances tableType =
    [d| instance PersistableWidth $tableType
        instance PersistableWidth (Entity $tableType) |]

defineFromSqlPersistValueInstances :: TypeQ -> Q [Dec]
defineFromSqlPersistValueInstances tableType =
    [d| instance FromSql PersistValue $tableType
        instance FromSql PersistValue (Entity $tableType) |]

mkPersistablePrimaryKey :: TypeQ -> Q [Dec]
mkPersistablePrimaryKey typ = do
    notNullD <- deriveNotNullType typ
    persistableD <- defineFromToSqlPersistValue typ
    scalarDegD <- defineScalarDegree typ
    showCTermSQLD <- mkShowConstantTermsSQL typ
    return $ notNullD ++ persistableD ++ scalarDegD ++ showCTermSQLD

mkShowConstantTermsSQL :: TypeQ -> Q [Dec]
mkShowConstantTermsSQL typ =
    [d|instance LiteralSQL $typ where
           showLiteral' = showLiteral' . PersistSql.fromSqlKey|]

mkFieldType :: FieldDef -> TypeQ
mkFieldType fd =
    case nullable . fieldAttrs $ fd of
        Nullable ByMaybeAttr -> conT ''Maybe `appT` typ
        _ -> typ
  where
    typ = ftToType . fieldType $ fd

-- | Generate 'FromSql' 'PersistValue' and 'ToSql' 'PersistValue' instances for 'PersistField' types.
defineFromToSqlPersistValue :: TypeQ -> Q [Dec]
defineFromToSqlPersistValue typ = do
    fromSqlI <-
        [d| instance FromSql PersistValue $typ where
                recordFromSql = valueRecordFromSql unsafePersistValueFromSql |]
    toSqlI <-
        [d| instance ToSql PersistValue $typ where
                recordToSql = valueRecordToSql toPersistValue |]
    return $ fromSqlI ++ toSqlI

unsafePersistValueFromSql :: PersistField a => PersistValue -> a
unsafePersistValueFromSql v =
    case fromPersistValue v of
        Left err -> error $ T.unpack err
        Right a -> a

persistValueTypesFromPersistFieldInstances
    :: [String] -- ^ blacklist types
    -> Q (M.Map Name TypeQ)
persistValueTypesFromPersistFieldInstances blacklist = do
    pf <- reify ''PersistField
    pfT <- [t|PersistField|]
    case pf of
       ClassI _ instances -> return . M.fromList $ mapMaybe (go pfT) instances
       unknown -> fail $ "persistValueTypesFromPersistFieldInstances: unknown declaration: " ++ show unknown
  where
    go pfT (InstanceD _ [] (AppT insT t@(ConT n)) [])
           | insT == pfT
          && nameBase n `notElem` blacklist = Just (n, return t)
    go _ _ = Nothing

persistableWidthTypes :: Q (M.Map Name TypeQ)
persistableWidthTypes =
    reify ''PersistableWidth >>= goI
  where
    unknownDecl decl = fail $ "persistableWidthTypes: Unknown declaration: " ++ show decl
    goI (ClassI _ instances) = return . M.fromList . mapMaybe goD $ instances
    goI unknown = unknownDecl unknown
    goD (InstanceD _ _cxt (AppT _insT a@(ConT n)) _defs) = Just (n, return a)
    goD _ = Nothing

derivePersistableInstancesFromPersistFieldInstances
    :: [String] -- ^ blacklist types
    -> Q [Dec]
derivePersistableInstancesFromPersistFieldInstances blacklist = do
    types <- persistValueTypesFromPersistFieldInstances blacklist
    pwts <- persistableWidthTypes
    ftsql <- concatMapTypes defineFromToSqlPersistValue types
    ws <- concatMapTypes deriveNotNullType $ types `M.difference` pwts
    return $ ftsql ++ ws
  where
    concatMapTypes :: (Q Type -> Q [Dec]) -> M.Map Name TypeQ -> Q [Dec]
    concatMapTypes f = fmap concat . mapM f . M.elems
