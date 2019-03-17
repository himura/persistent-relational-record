persistent-relational-record
============================

[![Travis](https://img.shields.io/travis/himura/persistent-relational-record/master.svg)](https://travis-ci.org/himura/persistent-relational-record)
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

Now, you can build queries in manner of HRR:

```Haskell
{-# LANGUAGE OverloadedLabels #-}

module Query where

import Data.Text (Text)
import Database.Persist (Entity)
import Database.Relational

import Model

selectUserByUserGroupNameList
    -> [Text] -- ^ list of userGroup name
    -> Relation () (Entity User)
selectUserByUserGroupNameList userGroupNames =
    relation $ do
        user <- query userTable
        userId <- query $ userIdFromUserGroupNameList userGroupNames
        on $ #id user .=. userId
        return user

-- ^ query user_id by userGroup name list
--
-- @
-- SELECT
--   user_id
-- FROM membership
-- INNER JOIN userGroup ON userGroup.id = membership.userGroup_id
-- WHERE userGroup.name IN (<<userGroupNames>>)
-- USERGROUP BY membership.user_id
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

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Database.Persist.MySQL
import Database.Persist.Relational
import Database.Relational

import Model
import Query

sample1 :: SqlPersistT (LoggingT IO) [UserId]
sample1 = runResourceT . runConduit $ runQuery (relationalQuery $ userIdFromUserGroupNameList ["tokyo", "haskell"]) () .| CL.consume

sample2 :: SqlPersistT (LoggingT IO) [Entity User]
sample2 = runResourceT . runConduit $ runQuery (relationalQuery $ selectUserByUserGroupNameList ["tokyo", "haskell"]) () .| CL.consume

main :: IO ()
main = runStderrLoggingT $ withMySQLPool defaultConnectInfo 10 $ runSqlPool $ do
    mapM_ (liftBase . print) =<< sample1
    mapM_ (liftBase . print) =<< sample2
```

`runQuery` run the HRR `Query` and gives the result as conduit `Source`.

For a full runnable example, see [examples](https://github.com/himura/persistent-relational-record/tree/master/examples/) directory.
