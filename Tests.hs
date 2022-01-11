module Tests(
  runAllTests
) where

import Creek
import Board
import Constants
import Utils

import Data.Matrix

testGetIndexOfFirstEmptyValue:: Bool
testGetIndexOfFirstEmptyValue = let
    numCols = 20
    numRows = 20
    testMatrix = fromList numRows numCols (repeat testValueField)

    indexes:: [((Int, Int), (Int, Int))]
    indexes = [(getIndexOfFirstEmptyField (setElem emptyValueField (row, col) testMatrix), (row,col)) |
              row <- [1..numRows], col <- [1..numCols]]
  in
    all (==True) (map indexesEqual indexes)

testGetIndexesOfNeighboringPoints:: ([Bool], Bool)
testGetIndexesOfNeighboringPoints = let
  numRows = 5
  numCols = 5

  --For point (1,1) it should return [(1,2),(2,1)]
  first = ((getIndexesOfNeighboringPoints (1,1) numCols numRows), [(1,2), (2,1)])
  --For point (1,5) it should return [(1,2),(2,1)]
  second = ((getIndexesOfNeighboringPoints (1,5) numCols numRows), [(1,4), (2,5)])
  --For point (5,1) it should return [(5,2), (1,4)]
  third = ((getIndexesOfNeighboringPoints (5,1) numCols numRows), [(5,2), (4,1)])
  --For point (5,5) it should return [(5,4), (4,5)]
  fourth = ((getIndexesOfNeighboringPoints (5,5) numCols numRows), [(5,4), (4,5)])
  --For point (3,1) it should return [(3,2), (2,1), (4,1)]
  fifth = ((getIndexesOfNeighboringPoints (3,1) numCols numRows), [(3,2), (2,1), (4,1)])
  --For point (1, 3) it should return [(1,2), (1,4), (2,3)]
  sixth = ((getIndexesOfNeighboringPoints (1,3) numCols numRows), [(1,2), (1,4), (2,3)])
  --For point (3, 3) it should return [(3,2), (3,4), (2,3), (4,3)]
  seventh = ((getIndexesOfNeighboringPoints (3,3) numCols numRows), [(3,2), (3,4), (2,3), (4,3)])

  results = (map tupleOfListOfTuplesEqual [first, second, third, fourth, fifth, sixth, seventh])
  in
    (results, (all (==True) results))

testGetIndexesOfEmptyNeighboringPointsAllEmptyFields::([Bool], Bool)
testGetIndexesOfEmptyNeighboringPointsAllEmptyFields = let
  numRows = 5
  numCols = 5
  emptyBoard:: Matrix Int
  emptyBoard = getEmptyBoard numRows numCols

--  For point (1,1) it should return [(1,2),(2,1)]
  first = ((getIndexesOfEmptyNeighboringPoints (1,1) emptyBoard numCols numRows), [(1,2), (2,1)])
  --For point (1,5) it should return [(1,2),(2,1)]
  second = ((getIndexesOfEmptyNeighboringPoints (1,5) emptyBoard numCols numRows), [(1,4), (2,5)])
  --For point (5,1) it should return [(5,2), (1,4)]
  third = ((getIndexesOfEmptyNeighboringPoints (5,1) emptyBoard numCols numRows), [(5,2), (4,1)])
  --For point (5,5) it should return [(5,4), (4,5)]
  fourth = ((getIndexesOfEmptyNeighboringPoints (5,5) emptyBoard numCols numRows), [(5,4), (4,5)])
  --For point (3,1) it should return [(3,2), (2,1), (4,1)]
  fifth = ((getIndexesOfEmptyNeighboringPoints (3,1) emptyBoard numCols numRows), [(3,2), (2,1), (4,1)])
  --For point (1, 3) it should return [(1,2), (1,4), (2,3)]
  sixth = ((getIndexesOfEmptyNeighboringPoints (1,3) emptyBoard numCols numRows), [(1,2), (1,4), (2,3)])
  --For point (3, 3) it should return [(3,2), (3,4), (2,3), (4,3)]
  seventh = ((getIndexesOfEmptyNeighboringPoints (3,3) emptyBoard numCols numRows), [(3,2), (3,4), (2,3), (4,3)])

  results = (map tupleOfListOfTuplesEqual [first, second, third, fourth, fifth, sixth, seventh])
  in
    (results, (all (==True) results))

testSetValuesUnderIndexesToTestValueAllIndexes::Bool
testSetValuesUnderIndexesToTestValueAllIndexes = let
    numRows = 5
    numCols = 5
    emptyBoard:: Matrix Int
    emptyBoard = getEmptyBoard numRows numCols

    matrixWithJustTestValue = fromList numRows numCols (repeat testValueField)

    allIndexes = [(row, col) | row <- [1..numRows], col <-[1..numCols]]

    outputMatrix = setValuesUnderIndexesToValue emptyBoard allIndexes testValueField

    in
      matrixWithJustTestValue == outputMatrix

testSetValuesUnderIndexesToTestValueNoIndexes::Bool
testSetValuesUnderIndexesToTestValueNoIndexes = let
    numRows = 5
    numCols = 5
    emptyBoard:: Matrix Int
    emptyBoard = getEmptyBoard numRows numCols

    outputMatrix = setValuesUnderIndexesToValue emptyBoard [] testValueField

    in
      emptyBoard == outputMatrix

testAreaGrowEmptyBoard:: Bool
testAreaGrowEmptyBoard = let
  numRows = 5
  numCols = 5

  allIndexes = [(row, col) | row <- [1..numRows], col <-[1..numCols]]

  emptyBoard:: Matrix Int
  emptyBoard = setValuesUnderIndexesToValue (getEmptyBoard numRows numCols) allIndexes testValueField
  matrixWithJustTestValue = fromList numRows numCols (repeat testValueField)
  outputMatrix = areaGrow (1, 1) emptyBoard
  in
    outputMatrix == matrixWithJustTestValue

testAreaGrowSplitInTwo:: Bool
testAreaGrowSplitInTwo = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    splitInTwoMatrix:: Matrix Int
    splitInTwoMatrix = fromLists (take 5 (repeat [a,a,b,a,a]))
    expectedOutput:: Matrix Int
    expectedOutput = fromLists (take 5 (repeat [c,c,b,a,a]))

    outputMatrix = areaGrow (1, 1) splitInTwoMatrix
  in
    outputMatrix == expectedOutput

testAreaGrowIdentity:: Bool
testAreaGrowIdentity = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    identityMatrix:: Matrix Int
    identityMatrix = fromLists [
      [b, a, a, a, a],
      [a, b, a, a, a],
      [a, a, b, a, a],
      [a, a, a, b, a],
      [a, a, a, a, b]
      ]
    expectedOutput:: Matrix Int
    expectedOutput = fromLists [
      [b, c, c, c, c],
      [a, b, c, c, c],
      [a, a, b, c, c],
      [a, a, a, b, c],
      [a, a, a, a, b]
      ]

    outputMatrix = areaGrow (getIndexOfFirstEmptyField identityMatrix) identityMatrix
  in
    outputMatrix == expectedOutput

testAreaGrowSingleChannel:: Bool
testAreaGrowSingleChannel = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    singleChannelMatrix:: Matrix Int
    singleChannelMatrix = fromLists [
      [a, a, a, a],
      [b, b, b, a],
      [b, b, a, a],
      [a, a, a, b]
      ]

    expectedOutput = fromLists [
        [c, c, c, c],
        [b, b, b, c],
        [b, b, c, c],
        [c, c, c, b]
        ]
    outputMatrix = areaGrow (getIndexOfFirstEmptyField singleChannelMatrix) singleChannelMatrix
  in
    outputMatrix == expectedOutput

testAreEmptyFieldsCreatingSingleAreaIdentityMatrix:: Bool
testAreEmptyFieldsCreatingSingleAreaIdentityMatrix = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  identityMatrix = fromLists [
        [b, a, a, a, a],
        [a, b, a, a, a],
        [a, a, b, a, a],
        [a, a, a, b, a],
        [a, a, a, a, b]
        ]
  in
    areEmptyFieldsCreatingSingleArea identityMatrix == False

testAreEmptyFieldsCreatingSingleAreaSingleChannelMatrix:: Bool
testAreEmptyFieldsCreatingSingleAreaSingleChannelMatrix = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  singleChannelMatrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]
  in
    areEmptyFieldsCreatingSingleArea singleChannelMatrix == True

testGetFieldsSurroundingIntersectionCorners:: ([Bool], Bool)
testGetFieldsSurroundingIntersectionCorners = let
  height = 4
  width = 4
  value = 10

  indexesList = [(i,j)| i <- [1..height], j <- [1..width]]
  indexesMatrix = fromList height width indexesList

  firstInter:: ((Int, Int), Int)
  firstInter = ((0,0),value)
  first = ((getFieldsSurroundingIntersection indexesMatrix firstInter), [(1,1)])

  secondInter:: ((Int, Int), Int)
  secondInter = ((0,4),value)
  second = ((getFieldsSurroundingIntersection indexesMatrix secondInter), [(1,4)])


  thirdInter:: ((Int, Int), Int)
  thirdInter = ((4,0),value)
  third = ((getFieldsSurroundingIntersection indexesMatrix thirdInter), [(4,1)])

  fourthInter:: ((Int, Int), Int)
  fourthInter = ((4,4),value)
  fourth = ((getFieldsSurroundingIntersection indexesMatrix fourthInter), [(4,4)])

  results = (map tupleOfListOfTuplesEqual [first, second, third, fourth])
    in
      (results, (all (==True) results))

testGetFieldsSurroundingIntersectionEdges:: ([Bool], Bool)
testGetFieldsSurroundingIntersectionEdges = let
  height = 4
  width = 4
  value = 10

  indexesList = [(i,j)| i <- [1..height], j <- [1..width]]
  indexesMatrix = fromList height width indexesList

  firstInter:: ((Int, Int), Int)
  firstInter = ((0,2),value)
  first = ((getFieldsSurroundingIntersection indexesMatrix firstInter), [(1,2), (1,3)])

  secondInter:: ((Int, Int), Int)
  secondInter = ((2, 0),value)
  second = ((getFieldsSurroundingIntersection indexesMatrix secondInter), [(2,1), (3,1)])

  thirdInter:: ((Int, Int), Int)
  thirdInter = ((4, 1),value)
  third = ((getFieldsSurroundingIntersection indexesMatrix thirdInter), [(4,1), (4,2)])

  fourthInter:: ((Int, Int), Int)
  fourthInter = ((1, 4),value)
  fourth = ((getFieldsSurroundingIntersection indexesMatrix fourthInter), [(1,4), (2,4)])

  results = (map tupleOfListOfTuplesEqual [first, second, third, fourth])
    in
      (results, (all (==True) results))

testGetFieldsSurroundingIntersectionMiddle:: ([Bool], Bool)
testGetFieldsSurroundingIntersectionMiddle = let
  height = 4
  width = 4
  value = 10

  indexesList = [(i,j)| i <- [1..height], j <- [1..width]]
  indexesMatrix = fromList height width indexesList

  firstInter:: ((Int, Int), Int)
  firstInter = ((1, 1),value)
  first = ((getFieldsSurroundingIntersection indexesMatrix firstInter), [(1,1), (1,2), (2,1), (2,2)])

  secondInter:: ((Int, Int), Int)
  secondInter = ((3, 3),value)
  second = ((getFieldsSurroundingIntersection indexesMatrix secondInter), [(3,3), (3,4), (4,3), (4,4)])

  results = (map tupleOfListOfTuplesEqual [first, second])
    in
      (results, (all (==True) results))

testIsBoardFilledForIntersectionOneFilled:: Bool
testIsBoardFilledForIntersectionOneFilled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [b, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((0,0), 1)
  in
    isBoardFilledForIntersection testedMatrix intersection == True

testIsBoardFilledForIntersectionTwoFilled:: Bool
testIsBoardFilledForIntersectionTwoFilled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, b, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((0,2), 2)
  in
    isBoardFilledForIntersection testedMatrix intersection == True

testIsBoardFilledForIntersectionThreeFilled:: Bool
testIsBoardFilledForIntersectionThreeFilled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, b, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((2,2), 3)
  in
    isBoardFilledForIntersection testedMatrix intersection == True

testIsBoardFilledForIntersectionFourFilled:: Bool
testIsBoardFilledForIntersectionFourFilled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, b, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((2,2), 4)
  in
    isBoardFilledForIntersection testedMatrix intersection == True

testIsBoardFilledForEveryIntersectionOneIntersection:: Bool
testIsBoardFilledForEveryIntersectionOneIntersection = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a, a],
        [a, b, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((1,1), 1)
  in
    isBoardFilledForEveryIntersection testedMatrix [intersection] == True

testIsBoardFilledForEveryIntersectionThreeIntersections:: Bool
testIsBoardFilledForEveryIntersectionThreeIntersections = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, a, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1,2), 2), ((2,3),2), ((5,5),1)]
  in
    isBoardFilledForEveryIntersection testedMatrix intersections == True

testIsBoardFilledForEveryIntersectionFullExample:: Bool
testIsBoardFilledForEveryIntersectionFullExample = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
  in
    isBoardFilledForEveryIntersection testedMatrix intersections == True

testIsValidSolutionFullExample::Bool
testIsValidSolutionFullExample = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  testedMatrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
  in
    isValidSolution testedMatrix  intersections == True

testSolveFullExample = let

  a = emptyValueField
  b = filledValueField
  c = testValueField

  expectedOutput = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]

  inputBoard = getEmptyBoard 4 4

  Just solution = solve inputBoard intersections []

  in
    expectedOutput == solution


runAllTests:: ([Bool], Bool)
runAllTests = let
  tests = [
    testGetIndexOfFirstEmptyValue,
    (snd testGetIndexesOfNeighboringPoints),
    (snd testGetIndexesOfEmptyNeighboringPointsAllEmptyFields),
    testSetValuesUnderIndexesToTestValueAllIndexes,
    testSetValuesUnderIndexesToTestValueNoIndexes,
    testAreaGrowEmptyBoard,
    testAreaGrowSplitInTwo,
    testAreaGrowIdentity,
    testAreaGrowSingleChannel,
    testAreEmptyFieldsCreatingSingleAreaIdentityMatrix,
    testAreEmptyFieldsCreatingSingleAreaSingleChannelMatrix,
    (snd testGetFieldsSurroundingIntersectionCorners),
    (snd testGetFieldsSurroundingIntersectionEdges),
    (snd testGetFieldsSurroundingIntersectionMiddle),
    testIsBoardFilledForIntersectionOneFilled,
    testIsBoardFilledForIntersectionTwoFilled,
    testIsBoardFilledForIntersectionThreeFilled,
    testIsBoardFilledForIntersectionFourFilled,
    testIsBoardFilledForEveryIntersectionOneIntersection,
    testIsBoardFilledForEveryIntersectionThreeIntersections,
    testIsBoardFilledForEveryIntersectionFullExample,
    testIsValidSolutionFullExample,
    testIsValidSolutionFullExample,
    testSolveFullExample]
  in
    (tests, (all (==True) tests))
