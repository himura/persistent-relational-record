{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Relational (mkHrrInstances)
import Database.Persist.TH
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "db", mkHrrInstances] [persistLowerCase|
Image
    hash       ByteString
    type       ImageType
    created_at UTCTime
    changed_at UTCTime
    UniqueImageHash hash
    deriving Show Eq
Tag
    name       Text
    description Text Maybe
    UniqueTagName name
    deriving Show Eq
ImageTag
    imageId    ImageId
    tagId      TagId
    UniqueImageTag imageId tagId
    deriving Show Eq
|]
