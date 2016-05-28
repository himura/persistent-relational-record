{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixtures.BlogSample.User where

import Data.Text (Text)
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (User)
import qualified Fixtures.BlogSample.Model as Model

defineTableFromPersistent ''Model.User db

