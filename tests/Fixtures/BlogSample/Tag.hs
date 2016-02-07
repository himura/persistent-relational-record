{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fixtures.BlogSample.Tag where

import Data.Text (Text)
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (Tag)
import Prelude hiding (id)

defineTableFromPersistent "test" "tag" db

