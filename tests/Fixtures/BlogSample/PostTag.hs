{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fixtures.BlogSample.PostTag where

import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (PostTag)

defineTableFromPersistent "post_tag" db

