{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber.FAZ where

import           Data.List
import qualified Data.Text            as T
import           Data.Time.Format
import           NLP.Grabber.Article
import           System.Locale
import           Text.XML.HXT.Core    hiding (when)

getArticle :: String -> IO (Maybe Article)
getArticle = mergeArticle getArticleEntry

getArticleEntry :: ArrowXml a => a XmlTree Article
getArticleEntry = proc x -> do
    title <- deep $ hasName "head" >>> getChildren >>> hasName "title" /> getText -< x
    einleitung <- deep $ hasName "div" >>> hasAttrValue "id" (=="artikelEinleitung") -< x
    dateAuthor <- deep $ hasName "span" >>> hasAttrValue "itemprop" (=="author") -< einleitung
    date <- deep $ hasName "span" >>> hasAttrValue "itemprop" (=="datePublished") >>> getAttrValue "content" -< dateAuthor
    author <- deep $ hasName "a" >>> hasAttrValue "href" ("/redaktion/" `isPrefixOf`) >>> getChildren >>> hasName "span" /> getText -< dateAuthor
    item <- deep $ hasName "div" >>> hasAttrValue "class" (== "FAZArtikelContent") -< x
    text <- deep $ hasName "p" >>> (neg autorenmodulFilter)//> getText -< item
    returnA -< Article (T.strip $ T.pack title) (Just $ T.strip $ T.pack author) (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" date) (T.pack text)
    where
        autorenmodulFilter = hasAttrValue "class" (== "AutorenModul")
