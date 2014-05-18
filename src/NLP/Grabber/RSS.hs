{-# LANGUAGE Arrows #-}
module NLP.Grabber.RSS where

import           Control.Applicative
import           Control.Category
import Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           NLP.Grabber.Download
import           Prelude              hiding (id, (.))
import           Text.XML.HXT.Core
import Control.Concurrent.Async (mapConcurrently)
import NLP.Database.Article
import qualified Database.Persist as DB
import qualified Data.Text as T
import Data.Text (Text)

-- | Returns all Articles of a RSS feed
getRSS :: Text -- ^ URL of the RSS Feed
          -> (Text -> IO (Maybe Article)) -- ^ Parser for one article
          -> IO [Article]
getRSS url parser = do
    urls <- getUrls url
    catMaybes <$> mapConcurrently parser urls

-- | Returns all new Articles of a RSS feed
getNewRSS :: DB.PersistQuery m => Text -- ^ URL of the feed
             -> (Text -> IO (Maybe Article)) -- ^ Parser
             -> m [Article]
getNewRSS url parser = do
  urls' <- liftIO $ getUrls url
  maybeUrls <- mapM isInDB urls'
  let urls=catMaybes maybeUrls
  articles <- liftIO $ mapConcurrently parser urls
  return $ catMaybes articles

-- Private stuff
  
getLinks :: ArrowXml a => a XmlTree Text
getLinks = proc x -> do
    item <- deep (hasName "item") -< x
    link <- getChildren >>> hasName "link" /> getText -< item
    returnA -< T.pack link

getUrls :: Text -> IO [Text]
getUrls url = do
    doc <- download False url
    runX $ doc >>> getLinks


isInDB :: DB.PersistQuery m => Text -> m (Maybe Text)
isInDB url = do
  cnt <- DB.count [ArticleUrl DB.==. url]
  case cnt of
    0 -> return $ Just url
    _ -> return Nothing
