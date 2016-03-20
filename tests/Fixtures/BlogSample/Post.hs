{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fixtures.BlogSample.Post where

import Data.Text (Text)
import Data.Time
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (Post)

defineTableFromPersistent "post" db

