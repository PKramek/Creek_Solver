import Creek
import Board
import Constants
import Utils
import Data.Maybe
import Data.Matrix

input = "Creek (5,5) [((0, 2),2),((1,4),3),((2, 1),3),((2, 3),1),((3, 2),2),((3, 4),4),((4, 1),3),((5, 3),1),((5,5), 1), ((0,0), 1)]"
creek = read input :: Creek

solveCreek:: Creek -> Maybe(Matrix Int)
solveCreek creek = let
  (height, width) = getSize creek
  board = getEmptyBoard height width
  sortedIntersections = getIntersectionsSortedByDigitDesc creek
  in
    solve board sortedIntersections []

solveAndReport :: Creek -> IO ()
solveAndReport creek =
    case solveCreek creek of
      Nothing -> do
        putStrLn "There is no solution"
      (Just solution) -> do
        putStrLn "Solution:"
        print solution

main = do
  creek <- fromFile
  solveAndReport creek