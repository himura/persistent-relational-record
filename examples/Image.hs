{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Image where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Relational
import Database.Record.FromSql (recordFromSql)
import Model hiding (Image)
import qualified Model
import Types

defineTableFromPersistent "image" db

instance ToPersistEntity Image.Image (Entity Model.Image) where
    recordFromSql' = Entity <$> recordFromSql <*> (Model.Image <$> recordFromSql <*> recordFromSql <*> recordFromSql <*> recordFromSql)
