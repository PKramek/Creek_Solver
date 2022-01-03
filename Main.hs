import Creek
import Board
import Constants

import Data.Matrix

input = "Creek (4, 4) [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]"
creek = read input :: Creek

empty_board:: Matrix Int
empty_board = get_empty_board (getHeight creek) (getWidth creek)

first_element = empty_board ! (1,1)
