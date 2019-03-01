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
import Database.Persist.Relational
import Database.Persist.TH
import Types
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkHrr, deriveGenericForEntityId] [persistLowerCase|
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
