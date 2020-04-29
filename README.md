persistent-relational-record
============================

![CI](https://github.com/himura/persistent-relational-record/workflows/CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/persistent-relational-record.svg?style=flat)](https://hackage.haskell.org/package/persistent-relational-record)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/persistent-relational-record.svg)](http://packdeps.haskellers.com/feed?needle=persistent-relational-record)

## About ##

persistent-relational-record build a bridge between [Haskell Relational Record](https://hackage.haskell.org/package/relational-query)
and [Persistent](http://hackage.haskell.org/package/persistent).
It uses the persistent entities definition instead of obtaining schema from DB at compilation time.

## Getting Started ##

If you already define an entities in persistent's manner, then you are almost ready to use this module.
The entities definition in the style of persistent-relational-record are shown below:

Model.hs:

```Haskell
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

module Model where

import Data.Text (Text)
import Database.Persist.Relational
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkHrr, deriveGenericForEntityId] [persistLowerCase|
User
    email       Text
    name        Text

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
```

The main difference from the persistent version is that `mkHrr` are added to the 1st argument of the `share` function.
`mkHrr` generates various instances and helper functions from the entities definition to cooperate with HRR.

NOTE: `deriveGenericForEntityId` is also appended to the list in above example.
Currently, persistent does not generate `Generic` instances for EntityID types (`UserId`, `UserGroupId`, and `MembershipId` in above example).
Haskell Relational Record rely on the Generic instance of record types, so persistent-relational-record provides `deriveGenericForEntityId` for convenience to derive `Generic` instance via `StandaloneDeriving`

Now, you can build queries in manner of HRR:

```Haskell
{-# LANGUAGE OverloadedLabels #-}

module Query where

import Data.Text (Text)
import Database.Persist (Entity)
import Database.Relational

import Model

-- ^ query users by UserGroup name list
--
-- @
-- SELECT
--   user.*
-- FROM user
-- INNER JOIN (
--   -- built by userIdFromUserGroupNameList
--   SELECT
--     user_id
--   FROM membership
--   INNER JOIN userGroup ON userGroup.id = membership.userGroup_id
--   WHERE userGroup.name IN (<<userGroupNames>>)
--   GROUP BY membership.user_id
--   HAVING COUNT(membership.user_id) = <<length userGroupNames>>
-- ) uid
-- ON user.id = uid.user_id
-- @
selectUserByUserGroupNameList
    -> [Text] -- ^ list of UserGroup name
    -> Relation () (Entity User)
selectUserByUserGroupNameList userGroupNames =
    relation $ do
        user <- query userTable
        userId <- query $ userIdFromUserGroupNameList userGroupNames
        on $ #id user .=. userId
        return user

-- ^ query user_id by UserGroup name list
--
-- @
-- SELECT
--   user_id
-- FROM membership
-- INNER JOIN userGroup ON userGroup.id = membership.userGroup_id
-- WHERE userGroup.name IN (<<userGroupNames>>)
-- GROUP BY membership.user_id
-- HAVING COUNT(membership.user_id) = <<length userGroupNames>>
-- @
userIdFromUserGroupNameList
    :: [Text] -- ^ list of userGroup name
    -> Relation () UserId
userIdFromUserGroupNameList userGroupNames =
    aggregateRelation $ do
        membership <- query membershipTable
        userGroup <- query userGroupTable
        on $ #id userGroup .=. #userGroupId membership
        wheres $ #name userGroup `in'` values userGroupNames
        g <- groupBy $ #userId membership
        let c = count $ (#userGroupId membership)
        having $ c .=. value (fromIntegral . length $ userGroupNames)
        return g
```

Finally, we can execute a query by runQuery:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Database.Persist.MySQL
import Database.Persist.Relational
import Database.Relational

import Model
import Query

sample :: SqlPersistT (LoggingT IO) [Entity User]
sample = runResourceT . runConduit $ runQuery (relationalQuery $ selectUserByUserGroupNameList True ["tokyo", "haskell"]) () .| CL.consume

main :: IO ()
main = runStderrLoggingT $ withMySQLPool defaultConnectInfo 10 $ runSqlPool $ do
    mapM_ (liftIO . print) =<< sample
```

`runQuery` run the HRR `Query` and gives the result as conduit `Source`.

For a full runnable example, see [examples](https://github.com/himura/persistent-relational-record/tree/master/examples/) directory.
