{-# LANGUAGE Arrows #-}
module NLP.Grabber.RSS where

import           Control.Applicative
import           Control.Category
import           Data.Maybe
import           NLP.Grabber.Article
import           NLP.Grabber.Download
import           Prelude              hiding (id, (.))
import           Text.XML.HXT.Core

getFeeds :: ArrowXml a => a XmlTree String
getFeeds = proc x -> do
    link <- getChildren >>> hasName "link" /> getText -< x
    returnA -< link


readRSS :: String -> (String -> IO (Maybe Article)) -> IO [Article]
readRSS url parser = do
    doc <- get True url
    urls <- (runX $ doc >>> getFeeds)
    catMaybes <$> mapM parser urls
