{-# LANGUAGE Arrows #-}
module NLP.Grabber.RSS where

import           Control.Applicative
import           Control.Category
import           Data.Maybe
import           NLP.Grabber.Download
import           Prelude              hiding (id, (.))
import           Text.XML.HXT.Core
import Control.Concurrent.Async (mapConcurrently)
import NLP.Types
import NLP.Types.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Reader


-- | Returns all Articles of a RSS feed
getRSS :: Text -- ^ URL of the RSS Feed
          -> (Text -> IO (Maybe Article)) -- ^ Parser for one article
          -> IO [Article]
getRSS url parser = do
    urls <- getUrls url
    catMaybes <$> mapConcurrently parser urls

-- | Insert all new articles in the DB and returns them
handleNewRSS :: Text -- ^ URL of the feed
             -> (Text -> IO (Maybe Article)) -- ^ Parser
             -> NLP [Article]
handleNewRSS url parser = do
  urls' <- liftIO $ getUrls url
  catMaybes <$> mapM (handleArticle parser) urls'

handleArticle :: (Text -> IO (Maybe Article)) -> Text -> NLP (Maybe Article)
handleArticle parser url = do
  exist <- existURL url
  if exist then return Nothing else
    do
      article <- liftIO $ parser url
      case article of
        Just a -> void $ insertUnique a
        Nothing -> return ()
      return article


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
