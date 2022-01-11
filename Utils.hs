module Utils(
  splitWhen,
  indexesEqual,
  tupleOfListOfTuplesEqual,
  uniqueCombinations,
  firstSatisfying,
  extractMaybeMatrix
) where

import Data.Matrix

splitWhen:: (a -> Bool) -> [a] -> ([a], [a])
splitWhen _ []  = ([],[])
splitWhen f xs = helperSplit f [] xs
  where
    helperSplit f acc []  = (acc, [])
    helperSplit f acc (x:xs)  | f x == False = helperSplit f (acc ++ [x]) xs
                              | otherwise = (acc, (x:xs))

indexesEqual::Eq a => ((a, a), (a, a)) -> Bool
indexesEqual ((a,b), (c, d)) = a == c && b == d

tupleOfListOfTuplesEqual:: ([(Int, Int)], [(Int, Int)]) -> Bool
tupleOfListOfTuplesEqual ([],[]) = True
tupleOfListOfTuplesEqual ([], y) = False
tupleOfListOfTuplesEqual (x, []) = False
tupleOfListOfTuplesEqual ((x:xs),(y:ys)) = (fst x) == (fst y) && (snd x) == (snd y) &&
 (tupleOfListOfTuplesEqual (xs, ys))

-- source: https://www.py4u.net/discuss/1984220
uniqueCombinations:: (Eq t, Num t) => t -> [a] -> [[a]]
uniqueCombinations 0 _ = [[]]
uniqueCombinations _ [] = []
uniqueCombinations n (x : xs) = map (x :) (uniqueCombinations (n - 1) xs) ++ uniqueCombinations n xs

firstSatisfying :: (a->Bool) -> [a] -> Maybe a
firstSatisfying f [] = Nothing
firstSatisfying f (x:xs)  | f x == True   = Just x
                          | otherwise     = firstSatisfying f xs

extractMaybeMatrix:: Maybe(Maybe(Matrix Int)) -> Maybe( Matrix Int)
extractMaybeMatrix (Just solution) = solution
extractMaybeMatrix Nothing = Nothing