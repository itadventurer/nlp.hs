{-# LANGUAGE OverloadedStrings #-}
module NLP.Grabber where

import Database.Persist.Sqlite
import Control.Applicative
import Data.Text (Text)

import NLP.Database.Article
import NLP.Grabber.RSS
import NLP.Grabber.FAZ
import NLP.Grabber.Heise

feeds :: [(Text, Text -> IO (Maybe Article))]
feeds=[
    ("http://www.heise.de/newsticker/heise-atom.xml",NLP.Grabber.Heise.getArticle)
  , ("http://www.faz.net/rss/aktuell/",NLP.Grabber.FAZ.getArticle)
  ]

grabNews :: Text -> IO ()
grabNews file =runSqlite file $ do
  runMigration migrateAll
  articles <- concat <$> (mapM (uncurry getNewRSS) feeds)
  _ <- insertMany articles
  return ()

