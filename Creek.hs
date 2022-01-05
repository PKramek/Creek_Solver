module Creek(Creek, fromStdInput, fromFile, getSize, getHeight, getWidth, getFields,
             getFieldsSortedByDigitAsc, getFieldsSortedByDigitDesc) where

import Utils
import Data.Function (on)
import Data.List (sortBy)

data Creek = Creek (Int, Int) [((Int, Int), Int)] deriving (Read, Show)

fromStdInput :: IO Creek
fromStdInput = do
  putStr "Serialized creek: "
  serialized_creek <- getLine
  let creek = read serialized_creek:: Creek

  return creek

fromFile :: IO Creek
fromFile = do
  putStr "Path to file containing serialized creek: "
  path <- getLine
  serialized_creek <- readFile path
  let creek = read serialized_creek:: Creek

  return creek

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

