{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tag where

import Data.Text (Text)
import Database.Persist.Relational
import qualified Model
import Model hiding (Tag)

defineTableFromPersistent ''Model.Tag db
