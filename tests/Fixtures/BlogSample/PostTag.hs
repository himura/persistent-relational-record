{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fixtures.BlogSample.PostTag where

import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (PostTag)
import Prelude hiding (id)

defineTableFromPersistent "test" "post_tag" db

