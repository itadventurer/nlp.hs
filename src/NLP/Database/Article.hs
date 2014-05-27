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
Article
  url    Text
  title  Text
  author Text Maybe
  date   UTCTime Maybe
  text   Text
  UniqueUrl url
  deriving Show
Word
  text  Text
  UniqueWord text
  deriving Show
ArticleWord
  article ArticleId
  word    WordId
  UniqueAW article word
  deriving Show
Trigramm
  word1 WordId
  word2 WordId
  word3 WordId
  UniqueTrigramm word1 word2 word3
  deriving Show
Synonym
  word1 WordId
  word2 WordId
  UinqueSynonym word1 word2
  deriving Show
|]
