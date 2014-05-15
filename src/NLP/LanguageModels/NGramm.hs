module NLP.LanguageModels.NGramm where
import NLP.Index.Index
import qualified Data.Map as M

getNgramms :: Ord a => Int -> [a] -> M.Map [a] Int
getNgramms n = countOccurrence . tokenizeNgramms n

tokenizeNgramms :: Int -> [a] -> [[a]]
tokenizeNgramms _ [] = []
tokenizeNgramms n xs@(_:rest) = take n xs : tokenizeNgramms n rest

trainingNgramms :: Ord a => Int -> [a] -> M.Map [a] Float
trainingNgramms n wrds = 
    let ngrammIndex = getNgramms n wrds
        wordIndex = countOccurrence wrds in
    M.mapWithKey (getProb wordIndex) ngrammIndex
    where
        getProb :: Ord a => M.Map a Int -> [a] -> Int -> Float
        getProb ws (w:_) occ = (fromIntegral occ) / (fromIntegral (ws M.! w))
        getProb _ [] _ = 0
