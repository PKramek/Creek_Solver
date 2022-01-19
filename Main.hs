import Creek
import Board
import Constants
import Utils
import Data.Maybe
import Data.Matrix

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