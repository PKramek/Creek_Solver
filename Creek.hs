module Creek(
  Creek,
  fromStdInput,
  fromFile,
  getSize,
  getHeight,
  getWidth,
  getIntersections,
  getIntersectionsSortedByDigitAsc,
  getIntersectionsSortedByDigitDesc
) where

import Utils
import Data.Function (on)
import Data.List (sortBy)

data Creek = Creek (Int, Int) [((Int, Int), Int)] deriving (Read, Show)

fromStdInput :: IO Creek
fromStdInput = do
  putStr "Serialized creek: "
  serializedCreek <- getLine
  let creek = read serializedCreek:: Creek

  return creek

fromFile :: IO Creek
fromFile = do
  putStr "Path to file containing serialized creek: "
  path <- getLine
  serializedCreek <- readFile path
  let creek = read serializedCreek:: Creek

  putStrLn ("File contained creek: " ++ (show creek))

  return creek

getSize:: Creek -> (Int, Int)
getSize (Creek size _ ) = size

getHeight:: Creek -> Int
getHeight (Creek (height, width) _) = height

getWidth:: Creek -> Int
getWidth (Creek (height, width) _) = width

getIntersections:: Creek -> [((Int, Int), Int)]
getIntersections (Creek _ fields) = fields

getIntersectionsSortedByDigitAsc:: Creek -> [((Int, Int), Int)]
getIntersectionsSortedByDigitAsc (Creek _ fields) = sortBy (compare `on` snd) fields

getIntersectionsSortedByDigitDesc:: Creek -> [((Int, Int), Int)]
getIntersectionsSortedByDigitDesc creek = reverse (getIntersectionsSortedByDigitAsc creek)
