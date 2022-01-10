module Utils(splitWhen, indexes_equal, tuple_of_list_of_tuples_equal, uniqueCombinations) where

splitWhen:: (a -> Bool) -> [a] -> ([a], [a])
splitWhen _ []  = ([],[])
splitWhen f xs = helperSplit f [] xs
  where
    helperSplit f acc []  = (acc, [])
    helperSplit f acc (x:xs)  | f x == False = helperSplit f (acc ++ [x]) xs
                              | otherwise = (acc, (x:xs))

indexes_equal::Eq a => ((a, a), (a, a)) -> Bool
indexes_equal ((a,b), (c, d)) = a == c && b == d

tuple_of_list_of_tuples_equal:: ([(Int, Int)], [(Int, Int)]) -> Bool
tuple_of_list_of_tuples_equal ([],[]) = True
tuple_of_list_of_tuples_equal ([], y) = False
tuple_of_list_of_tuples_equal (x, []) = False
tuple_of_list_of_tuples_equal ((x:xs),(y:ys)) = (fst x) == (fst y) && (snd x) == (snd y) &&
 (tuple_of_list_of_tuples_equal (xs, ys))

-- source: https://www.py4u.net/discuss/1984220
uniqueCombinations 0 _ = [[]]
uniqueCombinations _ [] = []
uniqueCombinations n (x : xs) = map (x :) (uniqueCombinations (n - 1) xs) ++ uniqueCombinations n xs

