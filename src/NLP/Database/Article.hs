{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module NLP.Database.Article where
import           Database.Persist.TH
import           Data.Text        (Text)
import           Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBAuthor sql=author
  name Text
  email Text default ''
  UniqueAuthor name email
  deriving Show
DBArticle sql=article
  url    Text
  title  Text
  author DBAuthorId Maybe
  date   UTCTime Maybe
  text   Text
  UniqueUrl url
  deriving Show
DBWord sql=word
  text  Text
  UniqueWord text
  deriving Show
DBArticleWord sql=article_word
  article DBArticleId
  word    DBWordId
  occurrence Int
  UniqueAW article word
  deriving Show
DBTrigramm sql=trigramm
  word1 DBWordId
  word2 DBWordId
  word3 DBWordId
  UniqueTrigramm word1 word2 word3
  deriving Show
DBArticleTrigramm sql=article_trigramm
  article DBArticleId
  trigramm DBTrigrammId
  occurrence Int
  UniqueArticleTrigramm article trigramm
  deriving Show
DBSynonym sql=synonym
  word1 DBWordId
  word2 DBWordId
  UinqueSynonym word1 word2
  deriving Show
|]
