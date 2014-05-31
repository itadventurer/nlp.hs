{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NLP.Grabber.Duden where

import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.HXT.Core hiding (when)
import NLP.Grabber.Download
import Data.List
import Data.Monoid
import NLP.Types
import Control.Monad.Reader
import Control.Monad.Logger
import NLP.Grabber.Wordlist
import NLP.Types.Monad


getAndInsertWords :: NLP ()
getAndInsertWords = do
  urls1 <- liftIO $ getDefinitionList "http://www.duden.de/definition"
  void $ mapConcurrentlyNLP handleStep1 urls1
  where
    handleStep1, handleStep2 :: Text -> NLP ()
    handleStep1 url = do
      urls2 <- liftIO $ getDefinitionList url
      void $ mapM handleStep2 urls2
    handleStep2 url = do
      wordUrls <- getWordList url
      void $ mapM handleWord wordUrls

globalizeUrl :: T.Text -> T.Text
globalizeUrl = ("http://www.duden.de" <>)

handleWord :: Text -> NLP ()
handleWord url = do
  w <- getWord url
  case w of
    Just word -> insertWord word
    Nothing -> return ()

getWordList :: Text -> NLP [Text]
getWordList url = do
  $(logInfo) ("Getting Wordlist from " <> url)
  doc <- liftIO $ download True url
  liftIO $ runX $ doc >>> getLinks
  where
    getLinks = proc x -> do
      nurl <- deep $ hasName "a" >>> hasAttrValue "href" ("/rechtschreibung/" `isPrefixOf`) >>> getAttrValue "href" -< x
      returnA -< globalizeUrl $ T.pack nurl
          

getDefinitionList :: Text -> IO [Text]
getDefinitionList url = do
  doc <- download True url
  runX $ doc >>> getLinks
  where
    getLinks = proc x -> do
      content <- deep $ hasName "ul" -< x
      nurl <- deep $ hasName "a" >>> hasAttrValue "href" ("/definition/" `isPrefixOf`) >>> getAttrValue "href" -< content
      returnA -< globalizeUrl $ T.pack nurl


getWord :: T.Text -> NLP (Maybe (Word, [Word]))
getWord url = do
  $(logInfo) ("Getting Word from " <> url)
  doc <- liftIO $ download True url
  word <- liftIO $ runX $ doc >>> findWord
  synonyms <- liftIO $ runX $ doc >>> findSynonyms
  case length word of
    0 -> return Nothing
    _ -> return $ Just (head word,synonyms)
  where
    findWord = proc x -> do
      word <- deepest $ hasName "div" >>> hasAttrValue "class" (=="breadcrumb") /> getText -< x
      returnA -< Word $ T.pack word
    findSynonyms = proc x -> do
      synonym <- deepest $ hasName "a" >>> hasAttrValue "meta-topic" (=="Synonym") /> getText -< x
      returnA -< Word $ T.pack synonym
