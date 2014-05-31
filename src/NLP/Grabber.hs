{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber where

import Control.Applicative
import Data.Text (Text)

import NLP.Grabber.RSS
import NLP.Grabber.FAZ
import NLP.Grabber.Heise
import NLP.Grabber.Spiegel
import NLP.Grabber.Zeit
import NLP.Types
import NLP.Types.Monad

feeds :: [(Text, Text -> IO (Maybe Article))]
feeds=[
    ("http://www.heise.de/newsticker/heise-atom.xml",NLP.Grabber.Heise.getArticle)
  , ("http://www.faz.net/rss/aktuell/",NLP.Grabber.FAZ.getArticle)
  , ("http://www.spiegel.de/index.rss", NLP.Grabber.Spiegel.getArticle)
  , ("http://newsfeed.zeit.de/index", NLP.Grabber.Zeit.getArticle)
  ]

grabNews :: NLP ()
grabNews = do
  _ <- concat <$> mapConcurrentlyNLP (uncurry handleNewRSS) feeds
  return ()
