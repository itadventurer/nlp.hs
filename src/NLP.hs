{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text                 as T
import           NLP.Grabber.Heise
import           NLP.Grabber.FAZ
import           NLP.Grabber.RSS
import           NLP.Index.Index
import           NLP.Index.InvertedIndex
import           NLP.LanguageModels.NGramm
import           NLP.Tokenizer
import NLP.Grabber.Article
import NLP.Database.Article
import NLP.Grabber
import NLP.Types
import Database.Persist.MySQL

dbCfg :: ConnectInfo
dbCfg = ConnectInfo "localhost" 3306 "nlp" "nlp" "nlp" [] "" Nothing

main :: IO ()
main =
  withMySQLPool dbCfg 10 $ \pool ->
    flip runNLP (NLPEnv dbCfg pool) $ do
      runDB $ runMigration migrateAll
      grabNews
