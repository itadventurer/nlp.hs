module NLP.Index.Index where

import qualified Data.Map.Strict as M

countOccurrence :: (Ord a) => [a] -> M.Map a Int
countOccurrence xs = M.fromListWith (+) . zip xs $ repeat 1

makeIndex :: (Ord a, Ord b) => M.Map a [b] -> M.Map a (M.Map b Int)
makeIndex = M.map countOccurrence

printMap :: (Ord k, Show k, Show v) => M.Map k v -> String
printMap m = unlines $ map (\(k,a) -> show k ++ ": " ++ show a) $ M.toList  m
