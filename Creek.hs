module Creek(Creek, getSize, getHeight, getWidth, getFields, getFieldsSortedByDigitAsc, getFieldsSortedByDigitDesc) where

import Utils
import Data.Function (on)
import Data.List (sortBy)

data Creek = Creek {
  size:: (Int, Int),
  fields :: [((Int, Int), Int)]
} deriving (Show)

instance Read Creek where
  readsPrec _ input =
    let
      (creek_str, rest) = splitWhen (==' ') input
      (size_str, fields_str) = splitWhen (=='[') rest
      (height, width) = read size_str :: (Int, Int)
      list_of_moves = read fields_str :: [((Int, Int), Int)]
    in
      if creek_str /= "Creek"
        then error "Input string should start with Creek"
      else
        [(Creek (height, width) list_of_moves, "")]

getSize:: Creek -> (Int, Int)
getSize (Creek size _ ) = size

getHeight:: Creek -> Int
getHeight (Creek (height, width) _) = height

getWidth:: Creek -> Int
getWidth (Creek (height, width) _) = width

getFields:: Creek -> [((Int, Int), Int)]
getFields (Creek _ fields) = fields

getFieldsSortedByDigitAsc:: Creek -> [((Int, Int), Int)]
getFieldsSortedByDigitAsc (Creek _ fields) = sortBy (compare `on` snd) fields

getFieldsSortedByDigitDesc:: Creek -> [((Int, Int), Int)]
getFieldsSortedByDigitDesc creek = reverse (getFieldsSortedByDigitAsc creek)