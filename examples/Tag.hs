{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tag where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Relational
import Database.Record.FromSql (recordFromSql)
import qualified Model
import Model hiding (Tag)

defineTableFromPersistent "tag" db

instance ToPersistEntity Tag.Tag (Entity Model.Tag) where
    recordFromSql' = Entity <$> recordFromSql <*> (Model.Tag <$> recordFromSql)
