{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber.Sueddeutsche where

import           Data.List
import qualified Data.Text            as T
import           Data.Time.Format
import           NLP.Grabber.Article
import NLP.Types
import           System.Locale
import           Text.XML.HXT.Core    hiding (when)
import Data.Text (Text)
import Data.Maybe

getArticle :: Text -> IO (Maybe Article)
getArticle url = do
    aa <- mergeArticle getAutorArticle url
    case aa of
      Nothing -> mergeArticle getArticleEntry url
      Just _ -> return aa

getAutorArticle :: ArrowXml a => a XmlTree Article
getAutorArticle = proc x -> do
    title <- deep $ hasName "ul" >>>hasAttrValue "class" (=="pagenav")>>>getChildren >>> hasName "li" >>>getChildren >>>hasName "h1"/> getText -<x
    date<- deep $ hasName "time"  >>>hasAttrValue "class" (=="timeformat")>>> getAttrValue "datetime"-< x
    text <- deep $ hasName "section" >>> hasAttrValue "class" (== "body") >>> getChildren>>> hasName "p" >>> neg (hasAttrValue "class" (== "anzeige")) />getText -< x
    author <- deep $ hasName "span" >>>hasAttr "data-abbr"/>getText -< x
    returnA -< Article "" (T.strip $ T.pack title) (Just $ fromAuthorName $ T.strip $ T.pack author) (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date) (T.pack text)


getArticleEntry :: ArrowXml a => a XmlTree Article
getArticleEntry = proc x -> do
  title <- deep $ hasName "ul" >>>hasAttrValue "class" (=="pagenav")>>>getChildren >>> hasName "li" >>>getChildren >>>hasName "h1"/> getText -<x
  date<- deep $ hasName "time"  >>>hasAttrValue "class" (=="timeformat")>>> getAttrValue "datetime"-< x
  text <- deep $ hasName "section" >>> hasAttrValue "class" (== "body") >>> getChildren >>> hasName "p" >>> neg (hasAttrValue "class" (== "anzeige")) />getText -< x
  returnA -< Article "" (T.strip $ T.pack title) (Nothing) (parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date) (T.pack text)


