module Database.Persist.Relational.Config
    ( TableVarNameConfig(..)
    , defaultTableVarNameConfig
    ) where

import Data.Char

data TableVarNameConfig = TableVarNameConfig
    { tableVarNameFromTableTypeName :: String -> String
    , tableOfVarNameFromTableTypeName :: String -> String
    , relationVarNameFromTableTypeName :: String -> String
    , insertVarNameFromTableTypeName :: String -> String
    , insertQueryVarNameFromTableTypeName :: String -> String
    , columnNameToLabelName :: String -> String
    , columnOffsetsVarNameFromTableTypeName :: String -> String
    , columnOffsetsEntityVarNameFromTableTypeName :: String -> String
    , columnOffsetsVarUseNewName :: Bool
    -- ^ A columnOffsets for `tableType` is needed to generate a columnOffsets for `Entity tableType`.
    -- However, the columnOffsets for `tableType` is not meaningful for others, because persistent-relational-record does not generate neither TableDerivable nor PI projections for `tableType`
    -- Therefore, we use `newName` in order to hide it from others if columnOffsetsVarUseNewName is set.
    }

defaultTableVarNameConfig :: TableVarNameConfig
defaultTableVarNameConfig = TableVarNameConfig
    { tableVarNameFromTableTypeName = toLowerCamel . (++ "_table")
    , tableOfVarNameFromTableTypeName = toLowerCamel . ("tableOf_" ++)
    , relationVarNameFromTableTypeName = toLowerCamel . ("relation_" ++)
    , insertVarNameFromTableTypeName = toLowerCamel . ("insert_" ++)
    , insertQueryVarNameFromTableTypeName = toLowerCamel . ("insertQuery_" ++)
    , columnOffsetsVarNameFromTableTypeName = toLowerCamel . ("columnOffsets_" ++)
    , columnOffsetsEntityVarNameFromTableTypeName = toLowerCamel . ("columnOffsetsEntity_" ++)
    , columnOffsetsVarUseNewName = True
    , columnNameToLabelName = toLowerCamel
    }

toLowerCamel :: String -> String
toLowerCamel [] = []
toLowerCamel (x:xs) = toLower x : go xs
  where
    go [] = []
    go ('_':c:cs) = toUpper c : go cs
    go (c:cs) = c : go cs
