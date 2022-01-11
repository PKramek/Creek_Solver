import Creek
import Board
import Constants
import Utils
import Data.Maybe
import Data.Matrix

input = "Creek (4, 4) [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]"
creek = read input :: Creek

solveCreek:: Creek -> Maybe(Matrix Int)
solveCreek creek = let
  (height, width) = getSize creek
  board = get_empty_board height width
  sortedIntersections = getIntersectionsSortedByDigitDesc creek
  in
    solve board sortedIntersections []

--solveAndReport :: Creek -> IO ()
solveAndReport creek =
    case solveCreek creek of
      Nothing -> do
        putStrLn "There is no solution"
      (Just solution) -> do
        putStrLn "Solution:"
        print solution
