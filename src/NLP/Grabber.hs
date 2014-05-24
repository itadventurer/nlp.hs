{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber where

import Control.Applicative
import Data.Text (Text)

import NLP.Database.Article
import NLP.Grabber.RSS
import NLP.Grabber.FAZ
import NLP.Grabber.Heise
import NLP.Types

feeds :: [(Text, Text -> IO (Maybe Article))]
feeds=[
    ("http://www.heise.de/newsticker/heise-atom.xml",NLP.Grabber.Heise.getArticle)
  , ("http://www.faz.net/rss/aktuell/",NLP.Grabber.FAZ.getArticle)
  ]

grabNews :: NLP ()
grabNews = do
  _ <- concat <$> mapM (uncurry handleNewRSS) feeds
  return ()
