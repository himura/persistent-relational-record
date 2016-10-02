{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.Relational
import Database.Relational.Query as HRR hiding (fromMaybe)
import System.Environment

import qualified Image
import qualified ImageTag
import Model
import qualified Tag
import Types

selectImageByTagNameList
    :: Bool -- ^ match any
    -> [Text] -- ^ list of tag name
    -> Relation () Image.Image
selectImageByTagNameList matchAny tagNames = relation $ do
    img <- query Image.image
    imgids <- query $ imageIdFromTagNameList matchAny tagNames
    on $ img ! Image.id' .=. imgids
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
    imgtag <- query $ ImageTag.imageTag
    tag <- query $ Tag.tag
    on $ tag ! Tag.id' .=. imgtag ! ImageTag.tagId'
    wheres $ tag ! Tag.name' `in'` values tagNames
    g <- groupBy $ imgtag ! ImageTag.imageId'
    let c = HRR.count $ imgtag ! ImageTag.imageId'
    having $
        if matchAny
            then c .>. value (0 :: Int)
            else c .=. value (fromIntegral . length $ tagNames)
    return g

tagListOfImage :: Relation ImageId Tag.Tag
tagListOfImage = relation' $ placeholder $ \ph -> do
    tag <- query Tag.tag
    imgtag <- query ImageTag.imageTag
    on $ tag ! Tag.id' .=. imgtag ! ImageTag.tagId'
    wheres $ imgtag ! ImageTag.imageId' .=. ph
    return tag

addImages :: TagId
          -> [Image]
          -> SqlPersistT (LoggingT IO) ()
addImages tagId images = do
    imgIds <- mapM insert images
    mapM_ (\imgId -> insert $ ImageTag imgId tagId) imgIds

printImage :: ByteString -> SqlPersistT (LoggingT IO) ()
printImage hkey =
    getBy (UniqueImageHash hkey) >>= \case
        Just (Entity k val) -> do
            liftBase $ print val
            runResourceT $
                runQuery (relationalQuery tagListOfImage) k
                $$ CL.mapM_ (liftBase . print)
        Nothing -> liftBase $ putStrLn "Image not found"

sample :: SqlPersistT (LoggingT IO) ()
sample = do
    runMigration migrateAll
    now <- liftBase getCurrentTime

    tagShinkuId <- insert $ Tag "shinku" (Just "二階堂真紅")
    addImages tagShinkuId
        [ Image "4f4221435f9c5c430db2b093c91b8f1f" PNG now now
        , Image "11eb1ee2b8f9b471f15d85fb784a8fd6" PNG now now
        , Image "7e11b84e04f181179cde72a5d0a5731f" PNG now now
        , Image "e10f6af40a80a7f5794ea0bdc66d4ae3" PNG now now
        ]

    tagMareId <- insert $ Tag "mare" Nothing
    addImages tagMareId
        [ Image "fbc717314b90afe6819d4593c583109a" PNG now now
        , Image "dc593d1c551f2a1ea85c2dc5521c7fdf" PNG now now
        ]

    runResourceT $
        runQuery (relationalQuery $ selectImageByTagNameList False ["shinku"]) ()
        $$ CL.mapM_ (liftBase . print)

    runResourceT $
        runQuery (relationalQuery $ imageIdFromTagNameList True ["shinku", "mare"]) ()
        $$ CL.mapM_ (liftBase . print)

    printImage "11eb1ee2b8f9b471f15d85fb784a8fd6"
    printImage "fbc717314b90afe6819d4593c583109a"

    mapM_ (liftBase . print) =<< run

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
