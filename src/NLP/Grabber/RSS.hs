{-# LANGUAGE Arrows #-}
module NLP.Grabber.RSS (getRSS) where

import           Control.Applicative
import           Control.Category
import           Data.Maybe
import           NLP.Grabber.Article
import           NLP.Grabber.Download
import           Prelude              hiding (id, (.))
import           Text.XML.HXT.Core
import Control.Concurrent.Async (mapConcurrently)

getLinks :: ArrowXml a => a XmlTree String
getLinks = proc x -> do
    item <- deep (hasName "item") -< x
    link <- getChildren >>> hasName "link" /> getText -< item
    returnA -< link

getUrls :: String -> IO [String]
getUrls url = do
    doc <- get False url
    urls <- (runX $ doc >>> getLinks)
    return urls


getRSS :: String -> (String -> IO (Maybe Article)) -> IO [Article]
getRSS url parser = do
    urls <- getUrls url
    catMaybes <$> mapConcurrently parser urls
