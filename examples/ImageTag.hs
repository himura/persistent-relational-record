{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ImageTag where

import Database.Persist.Relational
import Model hiding (ImageTag)

defineTableFromPersistent "image_tag" db
