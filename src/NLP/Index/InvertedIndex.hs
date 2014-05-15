module NLP.Index.InvertedIndex where
import qualified Data.Map.Strict as Map

type Index          a  = Map.Map a Int
type InvertedIndex  a b= Map.Map a (Index b)

-- Create an inverted index from a normal index
invertedIndex :: (Ord a, Ord b) => Map.Map b (Index a) -> InvertedIndex a b
invertedIndex = Map.map Map.fromList . Map.fromListWith (++) . regroup . expand . Map.toList
    where
        expand  = concatMap (\(key,value) -> zip (repeat key) $ Map.toList value)
        regroup = map (\(fname, (word, count)) -> (word, [(fname, count)]))


-- Print an inverted index
printInvertedIndex :: (Ord a, Show a, Ord b, Show b) => InvertedIndex a b -> String
printInvertedIndex invin = unlines $ toString $ Map.toList $ Map.map printRow invin
    where
        toString :: (Show a, Show b) => [(a,b)] -> [String]
        toString ((token,str):xs) =  (show token ++ ": " ++ show str) : (toString xs )
        toString [] = []
        printRow :: (Show a, Show b) => Map.Map a b -> String
        printRow mmap = unwords $ toStr $ Map.toList mmap
            where
                toStr ((fname,count):xs) = (show fname ++ (' ' : show count)) : (toStr xs)
                toStr [] = []
