{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Persist.Relational
import Model hiding (Image)
import Prelude hiding (id)
import Types

defineTableFromPersistent "test" "image" db
