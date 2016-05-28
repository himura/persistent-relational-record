{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixtures.BlogSample.Tag where

import Data.Text (Text)
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (Tag)
import qualified Fixtures.BlogSample.Model as Model

defineTableFromPersistent ''Model.Tag db

