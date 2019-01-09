{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Image where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Persist.Relational
import Model hiding (Image)
import qualified Model
import Types

defineTableFromPersistent ''Model.Image db
