{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tag where

import Config
import Data.Text (Text)
import Model hiding (Tag)
import Prelude hiding (id)

defineTable "tag" db
