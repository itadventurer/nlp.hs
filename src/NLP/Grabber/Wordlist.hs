module NLP.Grabber.Wordlist where

import System.IO
import qualified Data.Text as T
import NLP.Types
import NLP.Types.Monad
import Control.Monad.Reader
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
  let syn = map (Synonym word) synonyms
  mapM_ insertUnique syn
