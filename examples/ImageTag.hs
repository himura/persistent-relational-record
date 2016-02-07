{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ImageTag where

import Database.Persist.Relational
import Model hiding (ImageTag)
import Prelude hiding (id)

defineTableFromPersistent "test" "image_tag" db
