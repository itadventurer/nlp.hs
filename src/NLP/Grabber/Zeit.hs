{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber.Zeit where

import qualified Data.Text            as T
import           Data.Time.Format
import           NLP.Grabber.Article
import           System.Locale
import           Text.XML.HXT.Core    hiding (when)
import Data.Text (Text)
import NLP.Helpers
import NLP.Types

-- | parses an article from faz.net
getArticle :: Text -> IO (Maybe Article)
getArticle = mergeArticle getArticleEntry

getArticleEntry :: ArrowXml a => a XmlTree Article
getArticleEntry = proc x -> do
  htmlhead <- deep $ hasName "head" -< x
  title <- getChildren >>> hasName "title" /> getText -< htmlhead
  date <- getChildren >>> hasName "meta" >>> hasAttrValue "name" (=="last-modified") >>> getAttrValue "content" -< htmlhead
  let author = Nothing
  text <- deep $ hasName "div" >>> hasAttrValue "class" (=="article-body") >>> getChildren >>> hasName "p" /> getText -< x
  returnA -< Article "" (head $ T.split (=='|') $ toText title) author (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" date) (T.pack text)
