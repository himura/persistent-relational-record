{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Database.Persist.Relational
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Model
import Types

defineTableFromPersistent "image" db
