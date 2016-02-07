{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ImageTag where

import Config
import Model hiding (ImageTag)
import Prelude hiding (id)

defineTable "image_tag" db
