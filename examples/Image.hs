{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Image where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Persist.Relational
import Model (hoge)
import Types

defineTableFromPersistent' "image" hoge
