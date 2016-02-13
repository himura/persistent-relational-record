{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Config
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Model
import Types

defineTable "image" db
