{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Database.Persist.MySQL (ConnectInfo (..), defaultConnectInfo, withMySQLPool)
import Database.Persist.Relational
import Database.Persist.Sql
import Database.Relational (relationalQuery)
import System.Environment

import Model
import Query
import Types

addUser :: (MonadUnliftIO m, MonadLogger m) => User -> [UserGroupId] -> SqlPersistT m UserId
addUser user userGroupIds = do
    userId <- insert user
    insertMany_ $ map (Membership userId) userGroupIds
    return userId

sample :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m ()
sample = do
    runMigration migrateAll
    now <- liftIO getCurrentTime

    userGroupPersistentId <- insert $ UserGroup "persistent" Nothing
    userGroupHrrId <- insert $ UserGroup "haskell-relational-record" Nothing
    userGroupHaskellJpId <- insert $ UserGroup "haskell-jp" (Just "https://haskell.jp/")

    _ <- addUser (User "michael@example.com" "Michael" UserActive now now) [userGroupPersistentId]
    _ <- addUser (User "khibino@example.com" "Kei" UserActive now now) [userGroupHrrId, userGroupHaskellJpId]
    _ <- addUser (User "thimura@example.com" "Takahiro" UserSuspended now now) [userGroupPersistentId, userGroupHrrId, userGroupHaskellJpId]

    liftIO $ putStrLn "## haskell-jp users:"
    runResourceT . runConduit $
        runQuery (relationalQuery $ selectUserByUserGroupNameList False ["haskell-jp"]) ()
        .| CL.mapM_ (liftIO . printUser)

    liftIO $ putStrLn "## persistent users:"
    runResourceT . runConduit $
        runQuery (relationalQuery $ selectUserByUserGroupNameList False ["persistent"]) ()
        .| CL.mapM_ (liftIO . printUser)

    liftIO $ putStrLn "## both persistent and haskell-relational-record users:"

    runResourceT . runConduit $
        runQuery (relationalQuery $ selectUserByUserGroupNameList False ["persistent", "haskell-relational-record"]) ()
        .| CL.mapM_ (liftIO . printUser)

printUser :: Entity User -> IO ()
printUser (Entity k (User {..})) =
    T.putStrLn $ T.concat [ T.pack (show userId), ": ", userName, " <", userEmail, ">" ]
  where
    userId = fromSqlKey k

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
    host <- fromMaybe "localhost" `fmap` lookupEnv "MYSQL_HOST"
    user <- fromMaybe "travis" `fmap` lookupEnv "MYSQL_USER"
    pass <- fromMaybe "" `fmap`lookupEnv "MYSQL_PASS"
    return defaultConnectInfo
        { connectHost = host
        , connectUser = user
        , connectPassword = pass
        , connectDatabase = "test"
        }

main :: IO ()
main = do
    connInfo <- getConnectInfo
    runLoggingT $ withMySQLPool connInfo 10 $ runSqlPool sample
  where
    -- runLoggingT = runNoLoggingT
    runLoggingT = runStderrLoggingT
