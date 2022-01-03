module Utils(splitWhen) where

splitWhen:: (a -> Bool) -> [a] -> ([a], [a])
splitWhen _ []  = ([],[])
splitWhen f xs = helperSplit f [] xs
  where
    helperSplit f acc []  = (acc, [])
    helperSplit f acc (x:xs)  | f x == False = helperSplit f (acc ++ [x]) xs
                              | otherwise = (acc, (x:xs))