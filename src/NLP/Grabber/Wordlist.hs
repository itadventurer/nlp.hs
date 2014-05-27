module NLP.Grabber.Wordlist where

import System.IO
import NLP.Database.Article
import qualified Data.Text as T
import NLP.Types
import qualified Database.Persist as DB
import Control.Monad.Reader
import NLP.Database.Helpers
import Data.Maybe

-- | Parses a handle of the form
-- word synonym1 synonym2…
-- and insert it in the DB
parseHandle :: Handle -> NLP ()
parseHandle handle = do
  t <- liftIO $ hGetContents handle
  let l = map (map ( Word . T.pack) . words) (lines t)
      m = mapMaybe listToTuple l
  mapM_ insertWord m
  where
    listToTuple (x:xs) = Just (x,xs)
    listToTuple [] = Nothing


insertWord :: (Word,[Word]) -> NLP ()
insertWord (word,synonyms) = do
  w_key <- insertOrKey word
  s_keys <- mapM insertOrKey synonyms
  let w_synonyms = map (Synonym w_key) s_keys
  runDB $ mapM_ DB.insertUnique w_synonyms
  return ()
