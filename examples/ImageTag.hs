{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ImageTag where

import Database.Persist.Relational
import Model hiding (ImageTag)
import qualified Model

defineTableFromPersistent ''Model.ImageTag db
