{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Fixtures.BlogSample.Model where

import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Relational
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkHrr, deriveGenericForEntityId] [persistLowerCase|
User
    name    Text
    age     Int
    UniqueUserName name
    deriving Show Eq Generic
Post
    title   Text
    userId  UserId
    created UTCTime
    body    Text
    deriving Show Eq Generic
Tag
    name    Text
    UniqueTagName name
    deriving Show Eq Generic
PostTag
    postId  PostId
    tagId   TagId
    UniquePostTag postId tagId
    deriving Show Eq Generic
|]
