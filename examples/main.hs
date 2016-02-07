{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.Relational
import Database.Relational.Query as HRR hiding (fromMaybe)
import System.Environment
import Data.Maybe

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
imageIdFromTagNameList matchAny tagNames =
    aggregateRelation
    [ g
    | imgtag <- query $ ImageTag.imageTag
    , tag <- query $ Tag.tag
    , () <- on $ tag ! Tag.id' .=. imgtag ! ImageTag.tagId'
    , () <- wheres $ tag ! Tag.name' `in'` values tagNames
    , g <- groupBy $ imgtag ! ImageTag.imageId'
    , let c = HRR.count $ imgtag ! ImageTag.imageId'
    , () <- having $
            if matchAny
            then c .>. value (0 :: Int)
            else c .=. value (fromIntegral . length $ tagNames)
    ]

addImages :: TagId
          -> [Image]
          -> SqlPersistT (LoggingT IO) ()
addImages tagId images = do
    imgIds <- mapM insert images
    mapM_ (\imgId -> insert $ ImageTag imgId tagId) imgIds

sample :: SqlPersistT (LoggingT IO) ()
sample = do
    runMigration migrateAll
    now <- liftBase getCurrentTime

    tagShinkuId <- insert $ Tag "shinku"
    addImages tagShinkuId
        [ Image "4f4221435f9c5c430db2b093c91b8f1f" PNG now now
        , Image "11eb1ee2b8f9b471f15d85fb784a8fd6" PNG now now
        , Image "7e11b84e04f181179cde72a5d0a5731f" PNG now now
        , Image "e10f6af40a80a7f5794ea0bdc66d4ae3" PNG now now
        ]

    tagMareId <- insert $ Tag "mare"
    addImages tagMareId
        [ Image "fbc717314b90afe6819d4593c583109a" PNG now now
        , Image "dc593d1c551f2a1ea85c2dc5521c7fdf" PNG now now
        ]

    runResourceT $
        runQuery (relationalQuery $ selectImageByTagNameList False ["shinku"]) ()
        $$ CL.mapM_ (liftBase . print)

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
    host <- fromMaybe "localhost" <$> lookupEnv "MYSQL_HOST"
    user <- fromMaybe "travis" <$> lookupEnv "MYSQL_USER"
    pass <- fromMaybe "" <$> lookupEnv "MYSQL_PASS"
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
