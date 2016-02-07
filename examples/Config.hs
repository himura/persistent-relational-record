module Config
       ( defineTable
       ) where

import Database.Relational.Query
import Database.Persist.Relational
import Database.Persist
import Language.Haskell.TH

dbName :: String
dbName = "test"

defineTable :: String -> [EntityDef] -> Q [Dec]
defineTable = defineTableFromPersistent' config dbName
  where
    config = defaultConfig { normalizedTableName = False }
