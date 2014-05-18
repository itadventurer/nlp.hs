{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber.Heise (getArticle) where

import           Data.List
import qualified Data.Text            as T
import           Data.Time.Format
import           NLP.Grabber.Article
import           System.Locale
import           Text.XML.HXT.Core    hiding (when)
import NLP.Database.Article
import Data.Text (Text)

getArticle :: Text -> IO (Maybe Article)
getArticle = mergeArticle getArticleEntry

getArticleEntry :: ArrowXml a => a XmlTree Article
getArticleEntry = proc x -> do
    item <- deep (hasName "article") -< x
    title <- getChildren >>> hasName "h1" /> getText -< item
    date <- getChildren >>> hasName "p" >>> dateFilter /> getText -< item
    text <- deep $ hasName "p" >>> neg dateFilter //> getText -< item
    author <- deep $ hasName "span" >>> hasAttrValue "class" (=="ISI_IGNORE") >>> getChildren >>> hasName "a" >>> hasAttrValue "href" ("mailto:" `isPrefixOf`) >>> getAttrValue "title" -< item
    returnA -< Article "" (T.strip $ T.pack title) (Just $ T.strip $ T.pack author) (parseTime defaultTimeLocale "%d.%m.%Y %H:%M" date) (T.pack text)
    where
        dateFilter = hasAttrValue "class" (== "news_datum")
