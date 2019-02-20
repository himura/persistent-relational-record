{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Relational.TH
       where

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
import Database.Record.FromSql
import Database.Record.Persistable
    ( PersistableRecordWidth
    , ProductConst
    , genericFieldOffsets
    , getProductConst
    , runPersistableRecordWidth
    )
import Database.Record.TH (deriveNotNullType)
import Database.Record.ToSql
import Database.Relational
import Database.Relational.OverloadedProjection (HasProjection(..))
import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.TH (defineTableTypes, defineScalarDegree)
import Language.Haskell.TH
import Language.Haskell.TH.Name.CamelCase

mkHrr :: TableVarNameConfig -> [EntityDef] -> Q [Dec]
mkHrr conf = concatMapM (mkHrrForEntityDef conf)

mkHrrForEntityDef :: TableVarNameConfig -> EntityDef -> Q [Dec]
mkHrrForEntityDef conf ent = do
    tableConfig <- entityDefToTableConfig conf ent
    pkeyInstances <- mkPersistablePrimaryKey $ entityId ent
    tableTypes <- defineTableTypesFromTableConfig tableConfig
    columnOffsets <- defineColumnOffsetsVar tableConfig
    piProjections <- defineMonomorphicProjections tableConfig
    pwidthInstances <- definePersistableWidthInstances tableConfig
    fromSqlInstances <- defineFromSqlPersistValueInstances tableConfig

    return $ pkeyInstances ++ tableTypes ++ columnOffsets ++ piProjections ++ pwidthInstances ++ fromSqlInstances

data TableConfig = TableConfig
    { tableTypeName :: Name
    , tableDatabaseNameStr :: String
    , tableVarName :: Name
    , tableOfVarName :: Name
    , relationVarName :: Name
    , insertVarName :: Name
    , insertQueryVarName :: Name
    , columnOffsetsVarName :: Name
    , columnOffsetsEntityVarName :: Name
    , columns :: [Column]
    }

data Column = Column
    { columnDBName :: String
    , columnLabelName :: String
    , columnType :: TypeQ
    }

entityDefToTableConfig :: TableVarNameConfig -> EntityDef -> Q TableConfig
entityDefToTableConfig TableVarNameConfig {..} ent = do
    columnOffsetsVarName <-
        if columnOffsetsVarUseNewName
            then newName $ columnOffsetsVarNameFromTableTypeName tableTypeNameStr
            else return . mkName $ columnOffsetsVarNameFromTableTypeName tableTypeNameStr
    columnOffsetsEntityVarName <-
        if columnOffsetsVarUseNewName
            then newName $ columnOffsetsEntityVarNameFromTableTypeName tableTypeNameStr
            else return . mkName $ columnOffsetsEntityVarNameFromTableTypeName tableTypeNameStr
    return $ TableConfig {..}
  where
    tableTypeNameStr = T.unpack . unHaskellName . entityHaskell $ ent
    tableDatabaseNameStr = T.unpack . unDBName . entityDB $ ent
    tableTypeName = mkName tableTypeNameStr
    tableVarName = mkName $ tableVarNameFromTableTypeName tableTypeNameStr
    tableOfVarName = mkName $ tableOfVarNameFromTableTypeName tableTypeNameStr
    relationVarName = mkName $ relationVarNameFromTableTypeName tableTypeNameStr
    insertVarName = mkName $ insertVarNameFromTableTypeName tableTypeNameStr
    insertQueryVarName = mkName $ insertQueryVarNameFromTableTypeName tableTypeNameStr
    columns = map (\(name, typ) -> Column name (columnNameToLabelName name) typ) $ makeColumns ent

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

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM = (fmap concat .) . mapM

-- | Generate `HasProjection "columnName" (Entity table) columnType` instances
defineMonomorphicProjections :: TableConfig -> Q [Dec]
defineMonomorphicProjections tableConfig@TableConfig {columns} =
    concatMapM (definePiProjection tableConfig) $ zip [0..] columns

-- | Generate a `HasProjection "columnName" (Entity table) columnType` instance
definePiProjection ::
       TableConfig
    -> (Integer, Column) -- ^ (column idx, (column name, column type))
    -> Q [Dec]
definePiProjection TableConfig {tableTypeName, columnOffsetsVarName} (colIdx, Column {..}) =
    [d| instance HasProjection $(litT . strTyLit $ columnLabelName) (Entity $(conT tableTypeName)) $(columnType) where
            projection _ = definePi $ $(varE columnOffsetsVarName) Arr.! $(litE . integerL $ colIdx)
    |]

defineColumnOffsetsVar :: TableConfig -> Q [Dec]
defineColumnOffsetsVar TableConfig {tableTypeName, columnOffsetsVarName, columnOffsetsEntityVarName} = do
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

defineTableTypesFromTableConfig :: TableConfig -> Q [Dec]
defineTableTypesFromTableConfig TableConfig {..} =
    defineTableTypes
        (VarName tableOfVarName)
        (VarName tableVarName)
        (VarName insertVarName)
        (VarName insertQueryVarName)
        [t|Entity $(conT tableTypeName)|]
        tableDatabaseNameStr
        colNames
  where
    colNames = map columnDBName columns

definePersistableWidthInstances :: TableConfig -> Q [Dec]
definePersistableWidthInstances TableConfig{tableTypeName} =
    [d| instance PersistableWidth $(conT tableTypeName)
        instance PersistableWidth (Entity $(conT tableTypeName)) |]

defineFromSqlPersistValueInstances :: TableConfig -> Q [Dec]
defineFromSqlPersistValueInstances TableConfig{tableTypeName} =
    [d| instance FromSql PersistValue $(conT tableTypeName)
        instance FromSql PersistValue (Entity $(conT tableTypeName)) |]

mkHrrInstancesEachEntityDef :: EntityDef -> Q [Dec]
mkHrrInstancesEachEntityDef = mkPersistablePrimaryKey . entityId

mkPersistablePrimaryKey :: FieldDef -> Q [Dec]
mkPersistablePrimaryKey fd = do
    notNullD <- deriveNotNullType typ
    persistableD <- defineFromToSqlPersistValue typ
    scalarDegD <- defineScalarDegree typ
    showCTermSQLD <- mkShowConstantTermsSQL typ
    return $ notNullD ++ persistableD ++ scalarDegD ++ showCTermSQLD
  where
    typ = mkFieldType fd

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
