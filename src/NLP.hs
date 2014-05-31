{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text                 as T
import NLP.Grabber
import NLP.Article.Trigrammize
import Database.Persist.MySQL
import NLP.Grabber.Wordlist
import NLP.Database.Article
import NLP.Types.Monad
import NLP.Types

dbCfg :: ConnectInfo
dbCfg = ConnectInfo "localhost" 3306 "nlp" "nlp" "nlp" [] "" Nothing

main :: IO ()
main =
  withMySQLPool dbCfg 50 $ \pool ->
    flip runNLP (NLPEnv dbCfg pool) $ do
      runDB $ runMigration migrateAll
      -- _ <- grabNews
      _ <- handleAllArticles
      --h <- liftIO $ openFile "words" ReadMode
      -- parseHandle h
      -- _ <- forkNLP getAndInsertWords
      return ()
