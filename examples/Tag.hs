{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tag where

import Database.Persist.Relational
import Data.Text (Text)
import Model

defineTableFromPersistent "tag" db
