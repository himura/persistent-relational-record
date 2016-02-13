{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tag where

import Config
import Data.Text (Text)
import Model

defineTable "tag" db
