{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Relational.TH
       where

import Data.Array (Array, listArray, (!))
import Data.Int
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Quasi
import qualified Database.Persist.Sql as PersistSql
import Database.Record (PersistableWidth (..))
import Database.Record.FromSql
import Database.Record.Persistable (unsafePersistableRecordWidth)
import Database.Record.TH (deriveNotNullType, recordWidthTemplate, columnOffsetsVarNameDefault, makeRecordPersistableWithSqlType, persistableFunctionNamesDefault)
import Database.Record.ToSql
import Database.Relational.Query hiding ((!))
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.TH (defineScalarDegree, defineProductConstructorInstance)
import qualified Database.Relational.Query.Table as Table
import Language.Haskell.TH
import Language.Haskell.TH.Lib.Extra (integralE, simpleValD)
import Language.Haskell.TH.Name.CamelCase (VarName (..), toVarExp, varNameWithPrefix, varCamelcaseName, conCamelcaseName, toTypeCon, toDataCon, conName)

ftToType :: FieldType -> TypeQ
ftToType (FTTypeCon Nothing t) = conT $ mkName $ T.unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = conT ''Int64
ftToType (FTTypeCon (Just m) t) = conT $ mkName $ T.unpack $ T.concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `appT` ftToType y
ftToType (FTList x) = listT `appT` ftToType x

data TableData = TableData
    { tableName :: String
    , recordName :: Name
    , columns :: [(String, TypeQ)]
    }

mkTableData :: EntityDef -> TableData
mkTableData entity = TableData tn recn cs
  where
    tn = T.unpack . unDBName . entityDB $ entity
    recn = mkName . T.unpack . unHaskellName . entityHaskell $ entity
    cs = makeColumns entity

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

defineTableTypesFromEntityDef :: EntityDef -> Q [Dec]
defineTableTypesFromEntityDef entity = do
    tDeriv <- defineTableDerivations td
    colsDs <- defineColumnsDefault (mkName tableName) [t|Entity $(conT recordName)|] columns
    return $ tDeriv ++ colsDs
  where
    td@TableData {..} = mkTableData entity

defineTableDerivableInstance :: TableData -> Q [Dec]
defineTableDerivableInstance TableData {..} =
    [d| instance TableDerivable (Entity $(conT recordName)) where
            derivedTable = Table.table $(stringE tableName) $(listE $ map (stringE . fst) columns) |]

defineTableDerivations :: TableData -> Q [Dec]
defineTableDerivations TableData {..} = do
    tableDs <- simpleValD tableVar [t| Table $entityType |]
               [| derivedTable |]
    relDs <- simpleValD relVar   [t| Relation () $entityType |]
             [| derivedRelation |]
    insDs   <- simpleValD insVar   [t| Insert $entityType |]
               [| derivedInsert id' |]
    insQDs  <- simpleValD insQVar  [t| forall p . Relation p $entityType -> InsertQuery p |]
               [| derivedInsertQuery id' |]
    return $ concat [tableDs, relDs, insDs, insQDs]
  where
    entityType = [t|Entity $(conT recordName)|]
    tableVar = varName $ tableName `varNameWithPrefix` "tableOf"
    relVar = varName $ varCamelcaseName tableName
    insVar = varName $ tableName `varNameWithPrefix` "insert"
    insQVar = varName $ tableName `varNameWithPrefix` "insertQuery"

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

defineTableFromPersistent :: String -> [EntityDef] -> Q [Dec]
defineTableFromPersistent tableName entities =
    case filter ((== tableName) . T.unpack . unDBName . entityDB) entities of
        (t:_) -> defineTableTypesFromEntityDef t
        _ -> error $ "makeColumns: Table " ++ tableName ++ " not found"

maybeNullable :: FieldDef -> Bool
maybeNullable fd = nullable (fieldAttrs fd) == Nullable ByMaybeAttr

maybeTyp :: Bool -> TypeQ -> TypeQ
maybeTyp may typ | may = conT ''Maybe `appT` typ
                 | otherwise = typ

mkHrrInstances :: [EntityDef] -> Q [Dec]
mkHrrInstances entities =
    concat `fmap` mapM mkHrrInstancesEachEntityDef entities

-- | All templates depending on SQL value type with configured names.
makeRecordPersistableWithSqlTypeDefault'
    :: TypeQ      -- ^ SQL value type
    -> String     -- ^ Table name of database
    -> Int        -- ^ Count of record columns
    -> Q [Dec]    -- ^ Result declarations
makeRecordPersistableWithSqlTypeDefault' sqlValueType tableName =
    makeRecordPersistableWithSqlType
        sqlValueType
        (persistableFunctionNamesDefault . conName $ name)
        (toTypeCon name, toDataCon name)
  where
    name = conCamelcaseName tableName

mkHrrInstancesEachEntityDef :: EntityDef -> Q [Dec]
mkHrrInstancesEachEntityDef entity = do
    ppkey <- mkPersistablePrimaryKey . entityId $ entity
    offs <- defineColumnOffsets' recordName [t|Entity $recordType|] colTypes
    rconD <- defineProductConstructorInstance recordType (conE recordName) (tail colTypes)
    sqlvD  <- makeRecordPersistableWithSqlTypeDefault' [t| PersistValue |] tableName (length columns - 1)
    tDeriv <- defineTableDerivableInstance td
    return $ ppkey ++ offs ++ rconD ++ sqlvD ++ tDeriv
  where
    td@TableData {..} = mkTableData entity
    recordType = conT recordName
    colTypes = map snd columns

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
    [d|instance ShowConstantTermsSQL $typ where
           showConstantTermsSQL' = showConstantTermsSQL' . PersistSql.fromSqlKey|]

mkFieldType :: FieldDef -> TypeQ
mkFieldType fd = maybeTyp (maybeNullable fd) (ftToType . fieldType $ fd)

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
    go pfT (InstanceD [] (AppT insT t@(ConT n)) [])
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
    goD (InstanceD _cxt (AppT _insT a@(ConT n)) _defs) = Just (n, return a)
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
