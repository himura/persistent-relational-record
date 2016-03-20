{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fixtures.BlogSample.User where

import Data.Text (Text)
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (User)

defineTableFromPersistent "user" db

