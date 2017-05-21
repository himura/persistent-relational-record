{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fixtures.BlogSample.Post where

import Data.Text (Text)
import Data.Time
import Database.Persist.Relational
import Fixtures.BlogSample.Model hiding (Post)
import qualified Fixtures.BlogSample.Model as Model

defineTableFromPersistent ''Model.Post db

