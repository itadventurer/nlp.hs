module NLP.Grabber.Article where

import           Data.Text        (Text)
import           Data.Time.Clock
import           Data.Time.Format

data Article = Article {
      articleTitle  :: Text
    , articleAuthor :: Maybe Text
    , articleDate   :: Maybe UTCTime
    , articleText   :: Text
    } deriving Show
