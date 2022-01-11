import Creek
import Board
import Constants
import Utils
import Data.Maybe
import Data.Matrix

input = "Creek (4, 4) [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]"
creek = read input :: Creek

--solveCreek:: Creek -> Maybe(Matrix Int)
solveCreek creek = let
  (height, width) = getSize creek
  board = get_empty_board height width
  sortedIntersections = getIntersectionsSortedByDigitDesc creek
  in
    solve_debugging board sortedIntersections []

board = get_empty_board 4 4
intersections:: [((Int, Int), Int)]
intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]

Just solution = solve_debugging board intersections []