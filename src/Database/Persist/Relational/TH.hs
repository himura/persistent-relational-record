{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Persist.Relational.TH where

import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Database.Persist
    ( Entity
    , EntityNameDB(unEntityNameDB)
    , EntityNameHS(unEntityNameHS)
    , FieldNameDB(FieldNameDB)
    , FieldType(..)
    , IsNullable(Nullable)
    , PersistField(..)
    , PersistValue
    , WhyNullable(ByMaybeAttr)
    , fieldAttrsContainsNullable
    , getEntityDBName
    , getEntityHaskellName
    )
import Database.Persist.EntityDef.Internal (EntityDef(entityHaskell))
import Database.Persist.Quasi.Internal
    ( PrimarySpec(DefaultKey, NaturalKey, SurrogateKey)
    , UnboundEntityDef(unboundEntityDef, unboundEntityFields,
                 unboundPrimarySpec)
    , UnboundFieldDef(UnboundFieldDef, unboundFieldAttrs,
                unboundFieldNameDB, unboundFieldType)
    )
import Database.Persist.Relational.Config (NameConfig(..), defaultNameConfig)
import Database.Record.FromSql (FromSql(..), valueRecordFromSql)
import Database.Record.Persistable
    ( PersistableRecordWidth
    , PersistableWidth(..)
    , ProductConst
    , genericFieldOffsets
    , getProductConst
    , runPersistableRecordWidth
    )
import Database.Record.TH (deriveNotNullType)
import Database.Record.ToSql (ToSql(..), valueRecordToSql)
import Database.Relational (LiteralSQL(..))
import Database.Relational.OverloadedProjection (HasProjection(..))
import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.TH (defineHasNotNullKeyInstance, defineScalarDegree, defineTableTypes)
import GHC.Generics (Generic)
import Language.Haskell.TH
    ( Dec(InstanceD)
    , Info(ClassI)
    , Name
    , Q
    , Type(AppT, ConT)
    , TypeQ
    , appT
    , conK
    , conT
    , integerL
    , listT
    , litE
    , litT
    , mkName
    , nameBase
    , newName
    , normalB
    , promotedT
    , reify
    , sigD
    , strTyLit
    , valD
    , varE
    , varP
    )
import qualified Data.Array as Arr
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.Persist.Sql as PersistSql
import Language.Haskell.TH.Name.CamelCase (VarName(..), varCamelcaseName)

mkHrr :: [UnboundEntityDef] -> Q [Dec]
mkHrr = mkHrrWithConfig defaultNameConfig

mkHrrWithConfig :: NameConfig -> [UnboundEntityDef] -> Q [Dec]
mkHrrWithConfig conf = concatMapM (mkHrrForEntityDef conf)

mkHrrForEntityDef :: NameConfig -> UnboundEntityDef -> Q [Dec]
mkHrrForEntityDef conf ent = do
    pkeyInstances <- mkPersistablePrimaryKey tableDef
    tableTypes <- defineTableTypesForTableDef conf tableDef
    piProjections <- defineMonomorphicProjections tableDef
    pwidthInstances <- definePersistableWidthInstances tableType
    fromSqlInstances <- defineFromSqlPersistValueInstances tableType
    notNullD <- defineHasNotNullKeyInstance [t|Entity $tableType|] 0 -- Entity key is always not null and pos=0

    return $ pkeyInstances ++ tableTypes ++ piProjections ++ pwidthInstances ++ fromSqlInstances ++ notNullD
  where
    tableDef@TableDef {tableType} = entityDefToTableDef conf ent

-- | standalone deriving Generic instance for entity IDs.
--
-- Persistent does not derive Generic instances for `Key a`.
-- see https://github.com/yesodweb/persistent/pull/734#issuecomment-346696921
--     https://github.com/yesodweb/persistent/issues/578
deriveGenericForEntityId :: [UnboundEntityDef] -> Q [Dec]
deriveGenericForEntityId entityDefs =
    concatMapM mkDerivingGeneric $ map entityIdType entityDefs
  where
    mkDerivingGeneric typ = [d| deriving instance Generic $typ |]
    -- getIdFieldDef (EntityIdField idFieldDef) = [idFieldDef]
    -- getIdFieldDef (EntityIdNaturalKey CompositeDef {compositeFields}) = NonEmpty.toList compositeFields


data TableDef = TableDef
    { tableType :: TypeQ
    , tableTypeName :: String
    , tableDatabaseName :: String
    , tableIdColumns :: [Column]
    , tableColumns :: [Column]
    }

data Column = Column
    { columnDBName :: String
    , columnLabelName :: String
    , columnType :: TypeQ
    }

mkPrimaryKeyColumn :: NameConfig -> UnboundEntityDef  -> [Column]
mkPrimaryKeyColumn conf ent =
    case unboundPrimarySpec ent of
        NaturalKey nk -> error $ "Unsupported NaturalKey: " ++ show nk
        SurrogateKey sk -> error $ "Unsupported SurrogateKey: " ++ show sk
        DefaultKey (FieldNameDB dbName) ->
            [ Column
                  { columnDBName = T.unpack dbName
                  , columnLabelName = columnNameToLabelName conf $ T.unpack dbName
                  , columnType = entityIdType ent
                  }
            ]

entityIdType :: UnboundEntityDef -> TypeQ
entityIdType ent = conT ''PersistSql.Key `appT` entityDefConT ent

entityDefConT :: UnboundEntityDef -> TypeQ
entityDefConT = pure . conK . mkName . T.unpack . unEntityNameHS . entityHaskell . unboundEntityDef

entityDefToTableDef :: NameConfig -> UnboundEntityDef -> TableDef
entityDefToTableDef conf ent =
    TableDef
        { tableType = conT $ mkName typeName
        , tableTypeName = typeName
        , tableDatabaseName = tableName
        , tableIdColumns = mkPrimaryKeyColumn conf ent
        , tableColumns = map (makeColumn conf) $ unboundEntityFields ent
        }
  where
    typeName = T.unpack . unEntityNameHS . getEntityHaskellName $ unboundEntityDef ent
    tableName = T.unpack . unEntityNameDB . getEntityDBName $ unboundEntityDef ent

ftToType :: FieldType -> TypeQ
ftToType (FTTypeCon Nothing t) = conT $ mkName $ T.unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
ftToType (FTTypeCon (Just m) t) = conT $ mkName $ T.unpack $ T.concat [m, ".", t]
ftToType (FTTypePromoted t) = promotedT $ mkName $ T.unpack t
ftToType (FTApp x y) = ftToType x `appT` ftToType y
ftToType (FTList x) = listT `appT` ftToType x

makeColumn :: NameConfig -> UnboundFieldDef -> Column
makeColumn NameConfig{columnNameToLabelName} fd@UnboundFieldDef { unboundFieldNameDB = FieldNameDB dbName } =
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
defineMonomorphicProjections TableDef{tableTypeName = tableTypeNameStr, tableIdColumns, tableColumns} = do
    columnOffsetsVarName <- newName $ "columnOffsets" ++ tableTypeNameStr
    columnOffsetsEntityVarName <- newName $ "columnOffsetsEntity" ++ tableTypeNameStr
    coVar <- defineColumnOffsetsVar tableTypeName columnOffsetsVarName columnOffsetsEntityVarName
    piPrjs <- concatMapM (definePiProjection tableTypeName columnOffsetsEntityVarName) $ zip [0..] columns
    return $ coVar ++ piPrjs
  where
    tableTypeName = mkName tableTypeNameStr
    columns = tableIdColumns ++ tableColumns

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
    colNames = map columnDBName $ tableIdColumns ++ tableColumns


definePersistableWidthInstances :: TypeQ -> Q [Dec]
definePersistableWidthInstances tableType =
    [d| instance PersistableWidth $tableType
        instance PersistableWidth (Entity $tableType) |]

defineFromSqlPersistValueInstances :: TypeQ -> Q [Dec]
defineFromSqlPersistValueInstances tableType =
    [d| instance FromSql PersistValue $tableType
        instance FromSql PersistValue (Entity $tableType) |]

-- | Generate HRR instances for persistent's default primary key type
mkPersistablePrimaryKey :: TableDef -> Q [Dec]
mkPersistablePrimaryKey TableDef {tableIdColumns = [Column {columnType = typ}]} = do
    notNullD <- deriveNotNullType typ
    persistableD <- defineFromToSqlPersistValue typ
    scalarDegD <- defineScalarDegree typ
    showCTermSQLD <- mkShowConstantTermsSQL typ
    return $ notNullD ++ persistableD ++ scalarDegD ++ showCTermSQLD
mkPersistablePrimaryKey _ = pure []

mkShowConstantTermsSQL :: TypeQ -> Q [Dec]
mkShowConstantTermsSQL typ =
    [d|instance LiteralSQL $typ where
           showLiteral' = showLiteral' . PersistSql.fromSqlKey|]

mkFieldType :: UnboundFieldDef -> TypeQ
mkFieldType fd =
    case fieldAttrsContainsNullable $ unboundFieldAttrs fd of
        Nullable ByMaybeAttr -> conT ''Maybe `appT` typ
        _ -> typ
  where
    typ = ftToType . unboundFieldType $ fd

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
