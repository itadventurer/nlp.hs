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
|]
