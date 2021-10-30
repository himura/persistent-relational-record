{-# LANGUAGE OverloadedLabels #-}

module Query where

import Data.Text (Text)
import Database.Persist (Entity)
import Database.Relational

import Model

selectUserByUserGroupNameList
    :: Bool -- ^ match any
    -> [Text] -- ^ list of userGroup name
    -> Relation () (Entity User)
selectUserByUserGroupNameList matchAny userGroupNames =
    relation $ do
        user <- query userTable
        userId <- query $ userIdFromUserGroupNameList matchAny userGroupNames
        on $ #id user .=. userId
        return user

-- ^ query user_id by userGroup name list
--
-- @
-- SELECT
--   user_id
-- FROM membership
-- INNER JOIN user_group ON user_group.id = membership.user_group_id
-- WHERE user_group.name IN (<<UserGroupNames>>)
-- GROUP BY membership.user_id
-- HAVING COUNT(membership.user_group_id) = <<length UserGroupNames>>
-- @
userIdFromUserGroupNameList
    :: Bool -- ^ match any
    -> [Text] -- ^ list of UserGroup name
    -> Relation () UserId
userIdFromUserGroupNameList matchAny userGroupNames =
    aggregateRelation $ do
        membership <- query membershipTable
        userGroup <- query userGroupTable
        on $ #id userGroup .=. #userGroupId membership
        wheres $ #name userGroup `in'` values userGroupNames
        g <- groupBy $ #userId membership
        let c = count $ #userGroupId membership
        having $
            if matchAny
                then c .>. value (0 :: Int)
                else c .=. value (fromIntegral . length $ userGroupNames)
        return g

userGroupListOfUser :: Relation UserId (Entity UserGroup)
userGroupListOfUser =
    relation' $
    placeholder $ \ph -> do
        userGroup <- query userGroupTable
        membership <- query membershipTable
        on $ #id userGroup .=. #userGroupId membership
        wheres $ #userId membership .=. ph
        return userGroup
