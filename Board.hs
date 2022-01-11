module Board(
  getEmptyBoard,
  getIndexOfFirstEmptyField,
  getIndexesOfNeighboringPoints,
  getIndexesOfEmptyNeighboringPoints,
  setValuesUnderIndexesToValue,
  areaGrow,
  areEmptyFieldsCreatingSingleArea,
  getFieldsSurroundingIntersection,
  getValueUnderIndex,
  getValuesUnderIndexes,
  isBoardFilledForIntersection,
  isBoardFilledForEveryIntersection,
  isValidSolution,
  solve
 ) where

import Data.Matrix
import Data.List
import Data.Maybe
import Constants
import Creek
import Utils

getEmptyBoard:: Int -> Int -> Matrix Int
getEmptyBoard height width = matrix height width $ \(i,j) -> emptyValueField

getValueUnderIndex:: (Int, Int) -> Matrix Int -> Int
getValueUnderIndex index board = board ! (index)

getValuesUnderIndexes:: [(Int, Int) ]-> Matrix Int -> [Int]
getValuesUnderIndexes indexes board = map (\index -> getValueUnderIndex index board) indexes

setValuesUnderIndexesToValue:: Matrix Int -> [(Int, Int)] -> Int -> Matrix Int
setValuesUnderIndexesToValue matrix [] value= matrix
setValuesUnderIndexesToValue matrix (index:xs) value = setValuesUnderIndexesToValue (setElem value index matrix) xs value

getIndexOfFirstEmptyField:: Matrix Int -> (Int, Int)
getIndexOfFirstEmptyField matrix =let
    numColumns = fromIntegral (ncols matrix)
    numRows = fromIntegral (nrows matrix)
    matrixAsList = toList matrix
    indexInList = fromIntegral (getIndexFromMaybe (elemIndex emptyValueField matrixAsList))

    row = indexInList `div` numRows
    column = indexInList `mod` numColumns
  in
--  Indexes in matrix start at 1, not 0
    (row + 1, column + 1)

getIndexFromMaybe:: Maybe a -> a
getIndexFromMaybe Nothing = error "Index not found"
getIndexFromMaybe (Just index) = index

getIndexesOfNeighboringPoints:: (Int, Int) -> Int -> Int -> [(Int, Int)]
getIndexesOfNeighboringPoints (row, col) numColumns numRows =
  let
    connectedPoints = [(row, col-1), (row, col+1), (row-1, col), (row+1, col)]
  in
    filter (\(c, r) -> c >= 1 && r >= 1 && c <= numColumns && r <= numRows) connectedPoints

getIndexesOfEmptyNeighboringPoints:: (Int, Int) -> Matrix Int -> Int -> Int -> [(Int, Int)]
getIndexesOfEmptyNeighboringPoints (row, col) matrix numColumns numRows =
  let
    neighboringPoints = getIndexesOfNeighboringPoints (row, col) numColumns numRows
    valuesIndexesPairs = [((matrix ! index), index) |  index <- neighboringPoints]
  in
    [index | (value, index) <- valuesIndexesPairs, value == emptyValueField]

isValidSolution:: Matrix Int -> [((Int, Int), Int)] -> Bool
isValidSolution matrix intersections =
  (isBoardFilledForEveryIntersection matrix intersections) && (areEmptyFieldsCreatingSingleArea matrix)


areEmptyFieldsCreatingSingleArea:: Matrix Int -> Bool
areEmptyFieldsCreatingSingleArea matrix = let
  areaSeed = getIndexOfFirstEmptyField matrix
  matrixAfterAreaGrow = areaGrow areaSeed matrix
  asList = toList matrixAfterAreaGrow
  in
    (find (==emptyValueField) asList) == Nothing

areaGrow:: (Int, Int)-> Matrix Int -> Matrix Int
areaGrow seedIndex matrix = let
    numColumns = fromIntegral (ncols matrix)
    numRows = fromIntegral (nrows matrix)
    testedMatrix = setElem testValueField seedIndex matrix
  in
    if (matrix ! seedIndex) == filledValueField
      then error "Can`t start with that index, because there is filled value there"
    else
      areaGrowRecurrent [seedIndex] testedMatrix numColumns numRows


areaGrowRecurrent:: [(Int, Int)] -> Matrix Int -> Int -> Int -> Matrix Int
areaGrowRecurrent [] matrix numColumns numRows = matrix
areaGrowRecurrent seeds matrix numColumns numRows = let
    seed = head seeds
    remainingSeeds = tail seeds
    emptyNeighboursIndexes = getIndexesOfEmptyNeighboringPoints seed matrix numColumns numRows
    modifiedMatrix = setValuesUnderIndexesToValue matrix emptyNeighboursIndexes testValueField
  in
    areaGrowRecurrent (remainingSeeds ++ emptyNeighboursIndexes) modifiedMatrix numColumns numRows


isBoardFilledForIntersection:: Matrix Int -> ((Int, Int), Int) -> Bool
isBoardFilledForIntersection matrix ((x,y), value) =
  let
    indexesOfNeighbouringFields = getFieldsSurroundingIntersection matrix ((x,y), value)
    neighbouringValues = getValuesUnderIndexes indexesOfNeighbouringFields matrix
    numOfFilledValues = length (filter (==filledValueField) neighbouringValues)
  in
    value == numOfFilledValues

isBoardFilledForEveryIntersection :: Matrix Int -> [((Int, Int), Int)] -> Bool
isBoardFilledForEveryIntersection matrix intersections =
  all (==True) (map (\intersection ->  isBoardFilledForIntersection matrix intersection) intersections)

getFieldsSurroundingIntersection:: Matrix a -> ((Int, Int), Int) -> [(Int, Int)]
getFieldsSurroundingIntersection matrix ((x,y), _) =
  innerGetSurroundingIntersections (height, width) (x, y)
  where
    height = nrows matrix
    width = ncols matrix

    innerGetSurroundingIntersections:: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    innerGetSurroundingIntersections size (x, y)
      | x == 0      && y == 0       = [(1,1)]
      | x == height && y == 0       = [(height, 1)]
      | x == 0      && y == width   = [(1, width)]
      | x == height && y == width   = [(height, width)]
      | x == 0                      = [(x+1, y), (x+1, y+1)]
      | y == 0                      = [(x, y+1), (x+1, y+1)]
      | x == height                 = [(x, y), (x, y+1)]
      | y == width                  = [(x, y), (x+1, y)]
      | otherwise                   = [(xI, yI) | xI <- [x..(x+1)], yI <- [y..(y+1)]]

solve:: Matrix Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Maybe(Matrix Int)
solve matrix [] processedIntersections
  | isValidSolution matrix processedIntersections == True  = Just matrix
  | otherwise                                               = Nothing
solve matrix (intersection:intersections) processedIntersections =
  let
    (_, value) = intersection
    fieldsAroundIntersection = getFieldsSurroundingIntersection matrix intersection
    possibleMoves = uniqueCombinations value fieldsAroundIntersection
    everyPossibleBoard = map(\moves -> setValuesUnderIndexesToValue matrix moves filledValueField) possibleMoves

    solveTransform =
      (\transformedBoard -> solve transformedBoard intersections (intersection:processedIntersections))
    satisfyCondition = (\solution -> not (isNothing solution))
  in
    extractMaybeMatrix (firstSatisfying satisfyCondition (map (solveTransform) everyPossibleBoard))


