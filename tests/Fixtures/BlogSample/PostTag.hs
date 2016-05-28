{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixtures.BlogSample.PostTag where

import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (PostTag)
import qualified Fixtures.BlogSample.Model as Model

defineTableFromPersistent ''Model.PostTag db

