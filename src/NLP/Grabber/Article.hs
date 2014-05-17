{-# LANGUAGE Arrows            #-}
module NLP.Grabber.Article where

import           Data.Text        (Text)
import           Data.Time.Clock
import           Data.Time.Format
import           Text.XML.HXT.Core    hiding (when)
import           Control.Applicative
import           Data.Monoid
import qualified Data.Text            as T
import           NLP.Grabber.Download
{-
type FilterT = Control.Arrow.IOStateListArrow.IOSLA
       (Text.XML.HXT.Arrow.XmlState.TypeDefs.XIOState ())
       (Data.Tree.NTree.TypeDefs.NTree Text.XML.HXT.DOM.TypeDefs.XNode) Article-}

data Article = Article {
      articleTitle  :: Text
    , articleAuthor :: Maybe Text
    , articleDate   :: Maybe UTCTime
    , articleText   :: Text
    } deriving Show


mergeArticle f url= do
    doc <- get True url
    concatArticle <$> (runX $ doc >>> f)
    where
        concatArticle xs@(art:_) = Just $ art {articleText= mconcat $ map (T.strip . articleText) xs}
        concatArticle [] = Nothing
