{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.MySQL as Persist
import Database.Persist.Relational
#if MIN_VERSION_relational_query(0, 10, 0)
import Database.Relational as HRR hiding (fromMaybe, ($$))
#else
import Database.Relational.Query as HRR hiding (fromMaybe)
#endif
import System.Environment

import Model
import Types

selectImageByTagNameList
    :: Bool -- ^ match any
    -> [Text] -- ^ list of tag name
    -> Relation () (Entity Image)
selectImageByTagNameList matchAny tagNames = relation $ do
    img <- query imageTable
    imgids <- query $ imageIdFromTagNameList matchAny tagNames
    on $ #id img .=. imgids
    return img

-- ^ query ImageId by tag name list
--
-- @
-- SELECT image_id FROM image_tag
-- INNER JOIN tag ON tag.id = image_tag.tag_id
-- WHERE tag.name IN (<<tagNames>>)
-- GROUP BY image_tag.image_id
-- HAVING COUNT(image_tag.image_id) = <<length tagNames>>
-- @
imageIdFromTagNameList
    :: Bool -- ^ match any
    -> [Text] -- ^ list of tag name
    -> Relation () ImageId
imageIdFromTagNameList matchAny tagNames = aggregateRelation $ do
    imgtag <- query imageTagTable
    tag <- query tagTable
    on $ #id tag .=. #tagId imgtag
    wheres $ #name tag `in'` values tagNames
    g <- groupBy $ #imageId imgtag
    let c = HRR.count $ (#imageId imgtag :: Record Flat ImageId)
    having $
        if matchAny
            then c .>. value (0 :: Int)
            else c .=. value (fromIntegral . length $ tagNames)
    return g

tagListOfImage :: Relation ImageId (Entity Tag)
tagListOfImage = relation' $ placeholder $ \ph -> do
    tag <- query tagTable
    imgtag <- query imageTagTable
    on $ #id tag .=. #tagId imgtag
    wheres $ #imageId imgtag .=. ph
    return tag

addImages :: TagId
          -> [Image]
          -> SqlPersistT (LoggingT IO) ()
addImages tagId images = do
    imgIds <- mapM Persist.insert images
    mapM_ (\imgId -> Persist.insert $ ImageTag imgId tagId) imgIds

printImage :: ByteString -> SqlPersistT (LoggingT IO) ()
printImage hkey =
    getBy (UniqueImageHash hkey) >>= \case
        Just (Entity k val) -> do
            liftIO $ print val
            runResourceT $
                runQuery (relationalQuery tagListOfImage) k
                $$ CL.mapM_ (liftIO . print)
        Nothing -> liftIO $ putStrLn "Image not found"

sample :: SqlPersistT (LoggingT IO) ()
sample = do
    runMigration migrateAll
    now <- liftIO getCurrentTime

    tagShinkuId <- Persist.insert $ Tag "shinku" (Just "二階堂真紅")
    addImages tagShinkuId
        [ Image "4f4221435f9c5c430db2b093c91b8f1f" PNG now now
        , Image "11eb1ee2b8f9b471f15d85fb784a8fd6" PNG now now
        , Image "7e11b84e04f181179cde72a5d0a5731f" PNG now now
        , Image "e10f6af40a80a7f5794ea0bdc66d4ae3" PNG now now
        ]

    tagMareId <- Persist.insert $ Tag "mare" Nothing
    addImages tagMareId
        [ Image "fbc717314b90afe6819d4593c583109a" PNG now now
        , Image "dc593d1c551f2a1ea85c2dc5521c7fdf" PNG now now
        ]

    runResourceT $
        runQuery (relationalQuery $ selectImageByTagNameList False ["shinku"]) ()
        $$ CL.mapM_ (liftIO . print)

    runResourceT $
        runQuery (relationalQuery $ imageIdFromTagNameList True ["shinku", "mare"]) ()
        $$ CL.mapM_ (liftIO . print)

    printImage "11eb1ee2b8f9b471f15d85fb784a8fd6"
    printImage "fbc717314b90afe6819d4593c583109a"

    mapM_ (liftIO . print) =<< run

run :: SqlPersistT (LoggingT IO) [Entity Image]
run = runResourceT $
    runQuery (relationalQuery $ selectImageByTagNameList False ["shinku", "mare"]) () $$ CL.consume

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
    runStderrLoggingT $ withMySQLPool connInfo 10 $ runSqlPool sample
