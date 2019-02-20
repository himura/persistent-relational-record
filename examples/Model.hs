{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Relational (mkHrr, defaultTableVarNameConfig)
import Database.Persist.TH
import Types
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkHrr defaultTableVarNameConfig] [persistLowerCase|
Image
    hash       ByteString
    type       ImageType
    created_at UTCTime
    changed_at UTCTime
    UniqueImageHash hash
    deriving Show Eq Generic
Tag
    name       Text
    description Text Maybe
    UniqueTagName name
    deriving Show Eq Generic
ImageTag
    imageId    ImageId
    tagId      TagId
    UniqueImageTag imageId tagId
    deriving Show Eq Generic
|]

-- persistent does not derive the Generic instance for `Key XXX`.
--   https://github.com/yesodweb/persistent/issues/578
--   https://github.com/yesodweb/persistent/pull/734

deriving instance Generic ImageId
deriving instance Generic TagId
deriving instance Generic ImageTagId
