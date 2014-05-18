module NLP.Grabber.Article where

import           Text.XML.HXT.Core    hiding (when)
import           Control.Applicative
import           Data.Monoid
import qualified Data.Text            as T
import           NLP.Grabber.Download
import NLP.Database.Article
{-
type FilterT = Control.Arrow.IOStateListArrow.IOSLA
       (Text.XML.HXT.Arrow.XmlState.TypeDefs.XIOState ())
       (Data.Tree.NTree.TypeDefs.NTree Text.XML.HXT.DOM.TypeDefs.XNode) Article-}


mergeArticle f url= do
    doc <- download True url
    concatArticle <$> runX (doc >>> f)
    where
        concatArticle xs@(art:_) = Just $ art {articleUrl = url, articleText= mconcat $ map (T.strip . articleText) xs}
        concatArticle [] = Nothing
