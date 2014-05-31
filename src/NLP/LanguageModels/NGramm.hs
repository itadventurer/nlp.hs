module NLP.LanguageModels.NGramm where

import qualified Data.Map        as M
import           NLP.Index.Index
import Data.Text (Text)
import NLP.Database.Article
import NLP.Types
import Database.Persist
import Data.Maybe
import Control.Applicative
import Debug.Trace

getTrigramms :: [Text] -> M.Map Trigramm Int
getTrigramms txt =
  let ngrammMap = getNgramms 3 txt in
  M.mapKeys lToTri ngrammMap
  where
    lToTri :: [Text] -> Trigramm
    lToTri l = let [w1,w2,w3] = map Word l in
                                Trigramm w1 w2 w3

getNgramms :: Ord a => Int -> [a] -> M.Map [a] Int
getNgramms n = countOccurrence . tokenizeNgramms n

tokenizeNgramms :: Int -> [a] -> [[a]]
tokenizeNgramms _ [] = []
tokenizeNgramms n xs@(_:rest) = if length xs < n then [] else take n xs : tokenizeNgramms n rest

trainingNgramms :: Ord a => Int -> [a] -> M.Map [a] Float
trainingNgramms n wrds =
    let ngrammIndex = getNgramms n wrds
        wordIndex = countOccurrence wrds in
    M.mapWithKey (getProb wordIndex) ngrammIndex
    where
        -- | P(w_n | P(w_0,…,w_{n-1})
        getProb :: Ord a => M.Map a Int -> [a] -> Int -> Float
        getProb _ [] _ = 0
        getProb ix ws occ = fromIntegral occ / getProbPriori ix ws


        -- | Let ngramm=(w_0,w_1,…w_n). getProbPriori calculates P(w_0,…,w_{n-1})
        getProbPriori :: Ord a => M.Map a Int -> [a] -> Float
        getProbPriori ix ws = product $ map (\w -> fromIntegral (ix M.! w)) $ init ws
