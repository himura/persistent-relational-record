{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tag where

import Data.Text (Text)
import Database.Persist.Relational
import Model hiding (Tag)
import Prelude hiding (id)

defineTableFromPersistent "test" "tag" db
