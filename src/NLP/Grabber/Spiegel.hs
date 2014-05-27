{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber.Spiegel (getArticle) where

import           Data.List
import qualified Data.Text            as T
import           Data.Time.Format
import           NLP.Grabber.Article
import           System.Locale
import           Text.XML.HXT.Core    hiding (when)
import NLP.Database.Article
import Data.Text (Text)
import NLP.Helpers

getArticle :: Text -> IO (Maybe Article)
getArticle = mergeArticle getArticleEntry


getArticleEntry :: ArrowXml a => a XmlTree Article
getArticleEntry = proc x -> do
  title1 <- deep $ hasName "span" >>> hasAttrValue "class" (== "headline-intro") /> getText -< x
  title2 <- deep $ hasName "span" >>> hasAttrValue "class" (== "headline") /> getText -< x
  author <- deep $ hasName "a" >>> hasAttrValue "class" (== "autor-link") /> getText -< x
  date <- deep $ hasName "time" >>> hasAttrValue "class" (=="timeformat") >>> getAttrValue "datetime" -< x
  item <- deep $ hasName "div" >>> hasAttrValue "class" ("article-section" `isInfixOf`) -< x
  text <- deep $ hasName "p" /> getText -< item
  returnA -< Article "" (T.strip $ T.pack $ title1 ++ " " ++ title2) (Just $ toText author) (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date) (T.pack text)
                                                         
