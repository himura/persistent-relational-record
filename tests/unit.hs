{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

#if MIN_VERSION_relational_query(0, 10, 0)
import Database.Relational as HRR
#else
import Database.Relational.Query as HRR
#endif
import Fixtures.BlogSample.Model
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

case_simple_equal_string_query :: Assertion
case_simple_equal_string_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM user T0 WHERE (T0.name = 'testuser')"
  where
    subject = relation $ do
        user <- query userTable
        wheres $ #name user .=. value "testuser"
        return user

case_simple_equal_id_query :: Assertion
case_simple_equal_id_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM user T0 WHERE (T0.id = 1)"
  where
    subject = relation $ do
        user <- query userTable
        wheres $ #id user .=. value (UserKey 1)
        return user

case_simple_equal_placeholder :: Assertion
case_simple_equal_placeholder =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM user T0 WHERE (T0.id = ?)"
  where
    subject = relation' . placeholder $ \ph -> do
        user <- query userTable
        wheres $ #id user .=. ph
        return user

case_simple_comparing_int_query :: Assertion
case_simple_comparing_int_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.name AS f1, T0.age AS f2 FROM user T0 WHERE (T0.age >= 18)"
  where
    subject = relation $ do
        user <- query userTable
        wheres $ #age user .>=. value 18
        return user

case_inner_join_query :: Assertion
case_inner_join_query =
    show subject @?= "SELECT ALL T0.id AS f0, T0.title AS f1, T0.user_id AS f2, T0.created AS f3, T0.body AS f4 FROM post T0 INNER JOIN user T1 ON (T1.id = T0.user_id) WHERE (T1.age > 18)"
  where
    subject = relation $ do
        post <- query postTable
        user <- query userTable
        on $ #id user .=. #userId post
        wheres $ #age user .>. value 18
        return post

case_inner_join_with_subquery :: Assertion
case_inner_join_with_subquery =
    show subject @?= "SELECT ALL T0.id AS f0, T0.title AS f1, T0.user_id AS f2, T0.created AS f3, T0.body AS f4 FROM post T0 INNER JOIN (SELECT ALL T2.post_id AS f0 FROM tag T1 INNER JOIN post_tag T2 ON (T1.id = T2.tag_id) WHERE (T1.name IN ('haskell', 'hrr')) GROUP BY T2.post_id HAVING (COUNT(T2.post_id) = 2)) T3 ON (T0.id = T3.f0)"
  where
    subject = relation $ do
        post <- query postTable
        postids <- query $ postIdsFromTagNameList ["haskell", "hrr"]
        on $ #id post .=. postids
        return post

    postIdsFromTagNameList taglist = aggregateRelation $ do
        tag <- query tagTable
        posttag <- query postTagTable
        on $ #id tag .=. #tagId posttag
        wheres $ #name tag `in'` values taglist
        g <- groupBy $ #postId posttag
        let c = HRR.count $ #postId posttag
        having $ c .=. value (length taglist)
        return g

main :: IO ()
main = defaultMain [$testGroupGenerator]
