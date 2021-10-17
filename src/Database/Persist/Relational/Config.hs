module Database.Persist.Relational.Config
    ( NameConfig(..)
    , defaultNameConfig
    ) where

import Data.Char (toLower, toUpper)

data NameConfig = NameConfig
    { tableVarNameFromTableTypeName :: String -> String
    , insertVarNameFromTableTypeName :: String -> String
    , insertQueryVarNameFromTableTypeName :: String -> String
    , columnNameToLabelName :: String -> String
    }

defaultNameConfig :: NameConfig
defaultNameConfig = NameConfig
    { tableVarNameFromTableTypeName = toLowerCamel . (++ "_table")
    , insertVarNameFromTableTypeName = toLowerCamel . ("insert_" ++)
    , insertQueryVarNameFromTableTypeName = toLowerCamel . ("insertQuery_" ++)
    , columnNameToLabelName = toLowerCamel
    }

toLowerCamel :: String -> String
toLowerCamel [] = []
toLowerCamel (x:xs) = toLower x : go xs
  where
    go [] = []
    go ('_':c:cs) = toUpper c : go cs
    go (c:cs) = c : go cs
