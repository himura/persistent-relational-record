{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Database.Relational.Query as HRR
import Fixtures.BlogSample.Model
import qualified Fixtures.BlogSample.Post as Post
import qualified Fixtures.BlogSample.PostTag as PostTag
import qualified Fixtures.BlogSample.Tag as Tag
import qualified Fixtures.BlogSample.User as User
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

case_simple_equal_string_query :: Assertion
case_simple_equal_string_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM TEST.user T0 WHERE (T0.name = 'testuser')"
  where
    subject = relation $ do
        user <- query User.user
        wheres $ user ! User.name' .=. value "testuser"
        return user

case_simple_equal_id_query :: Assertion
case_simple_equal_id_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM TEST.user T0 WHERE (T0.id = 1)"
  where
    subject = relation $ do
        user <- query User.user
        wheres $ user ! User.id' .=. value (UserKey 1)
        return user

case_simple_comparing_int_query :: Assertion
case_simple_comparing_int_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM TEST.user T0 WHERE (T0.age >= 18)"
  where
    subject = relation $ do
        user <- query User.user
        wheres $ user ! User.age' .>=. value 18
        return user

case_inner_join_query :: Assertion
case_inner_join_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.title AS f1, T0.user_id AS f2, T0.created AS f3, T0.body AS f4 \
                     \FROM TEST.post T0 INNER JOIN TEST.user T1 ON (T1.id = T0.user_id) WHERE (T1.age > 18)"
  where
    subject = relation $ do
        post <- query Post.post
        user <- query User.user
        on $ user ! User.id' .=. post ! Post.userId'
        wheres $ user ! User.age' .>. value 18
        return post

case_inner_join_with_subquery :: Assertion
case_inner_join_with_subquery =
    show subject @?= "SELECT ALL T0.id AS f0, T0.title AS f1, T0.user_id AS f2, T0.created AS f3, T0.body AS f4 \
                     \FROM TEST.post T0 \
                     \INNER JOIN (SELECT ALL T2.post_id AS f0 FROM TEST.tag T1 \
                                 \INNER JOIN TEST.post_tag T2 ON (T1.id = T2.tag_id) \
                                 \WHERE (T1.name IN ('haskell', 'hrr')) \
                                 \GROUP BY T2.post_id HAVING (COUNT(T2.post_id) = 2)) T3 \
                     \ON (T0.id = T3.f0)"
  where
    subject = relation $ do
        post <- query Post.post
        postids <- query $ postIdsFromTagNameList ["haskell", "hrr"]
        on $ post ! Post.id' .=. postids
        return post

    postIdsFromTagNameList taglist = aggregateRelation $ do
        tag <- query Tag.tag
        posttag <- query PostTag.postTag
        on $ tag ! Tag.id' .=. posttag ! PostTag.tagId'
        wheres $ tag ! Tag.name' `in'` values taglist
        g <- groupBy $ posttag ! PostTag.postId'
        let c = HRR.count $ posttag ! PostTag.postId'
        having $ c .=. value (length taglist)
        return g

main :: IO ()
main = defaultMain [$testGroupGenerator]
