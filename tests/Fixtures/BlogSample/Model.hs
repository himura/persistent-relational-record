{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Fixtures.BlogSample.Model where

import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Relational

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "db", mkHrrInstances] [persistLowerCase|
User
    name    Text
    age     Int
    UniqueUserName name
    deriving Show Eq
Post
    title   Text
    userId  UserId
    created UTCTime
    body    Text
    deriving Show Eq
Tag
    name    Text
    UniqueTagName name
    deriving Show Eq
PostTag
    postId  PostId
    tagId   TagId
    UniquePostTag postId tagId
    deriving Show Eq
|]
