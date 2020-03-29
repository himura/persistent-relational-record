{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Relational
import Database.Persist.TH
import GHC.Generics
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkHrr, deriveGenericForEntityId] [persistLowerCase|
User
    email       Text
    name        Text
    status      UserStatus
    created_at  UTCTime
    updated_at  UTCTime

    UniqueUserEmail email
    deriving Show Eq Generic

UserGroup
    name        Text
    pageUrl     Text Maybe

    UniqueUserGroupName  name
    deriving Show Eq Generic

Membership
    userId      UserId
    userGroupId UserGroupId

    UniqueImageTag userId userGroupId
    deriving Show Eq Generic
|]
