module Board(
  get_empty_board,
  get_index_of_first_empty_field,
  get_indexes_of_neighboring_points,
  get_indexes_of_empty_neighboring_points,
  set_values_under_indexes_to_value,
  area_grow,
  are_empty_fields_creating_single_area,
  getFieldsSurroundingIntersection,
  get_value_under_index,
  get_values_under_indexes,
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

get_empty_board:: Int -> Int -> Matrix Int
get_empty_board height width = matrix height width $ \(i,j) -> emptyValueField

getIndexFromMaybe:: Maybe a -> a
getIndexFromMaybe Nothing = error "Index not found"
getIndexFromMaybe (Just index) = index

get_index_of_first_empty_field:: Matrix Int -> (Int, Int)
get_index_of_first_empty_field matrix =let
    num_columns = fromIntegral (ncols matrix)
    num_rows = fromIntegral (nrows matrix)
    matrix_as_list = toList matrix
    index_in_list = fromIntegral (getIndexFromMaybe (elemIndex emptyValueField matrix_as_list))

    row = index_in_list `div` num_rows
    column = index_in_list `mod` num_columns
  in
    (row + 1, column + 1)

get_indexes_of_neighboring_points:: (Int, Int) -> Int -> Int -> [(Int, Int)]
get_indexes_of_neighboring_points (row, col) num_columns num_rows =
  let
    connected_points = [(row, col-1), (row, col+1), (row-1, col), (row+1, col)]
  in
    filter (\(c, r) -> c >= 1 && r >= 1 && c <= num_columns && r <= num_rows) connected_points

get_indexes_of_empty_neighboring_points:: (Int, Int) -> Matrix Int -> Int -> Int -> [(Int, Int)]
get_indexes_of_empty_neighboring_points (row, col) matrix num_columns num_rows =
  let
    neighboring_points = get_indexes_of_neighboring_points (row, col) num_columns num_rows
    values_indexes_pairs = [((matrix ! index), index) |  index <- neighboring_points]
  in
    [index | (value, index) <- values_indexes_pairs, value == emptyValueField]

get_value_under_index:: (Int, Int) -> Matrix Int -> Int
get_value_under_index index board = board ! (index)

get_values_under_indexes:: [(Int, Int) ]-> Matrix Int -> [Int]
get_values_under_indexes indexes board = map (\index -> get_value_under_index index board) indexes

set_values_under_indexes_to_value:: Matrix Int -> [(Int, Int)] -> Int -> Matrix Int
set_values_under_indexes_to_value matrix [] value= matrix
set_values_under_indexes_to_value matrix (index:xs) value = set_values_under_indexes_to_value (setElem value index matrix) xs value

area_grow:: (Int, Int)-> Matrix Int -> Matrix Int
area_grow seed_index matrix = let
    num_columns = fromIntegral (ncols matrix)
    num_rows = fromIntegral (nrows matrix)
    tested_matrix = setElem testValueField seed_index matrix
  in
    if (matrix ! seed_index) == filledValueField
      then error "Can`t start with that index, because there is filled value there"
    else
      area_grow_recurrent [seed_index] tested_matrix num_columns num_rows


area_grow_recurrent:: [(Int, Int)] -> Matrix Int -> Int -> Int -> Matrix Int
area_grow_recurrent [] matrix num_columns num_rows = matrix
area_grow_recurrent seeds matrix num_columns num_rows = let
    seed = head seeds
    remaining_seeds = tail seeds
    empty_neighbours_indexes = get_indexes_of_empty_neighboring_points seed matrix num_columns num_rows
    modified_matrix = set_values_under_indexes_to_value matrix empty_neighbours_indexes testValueField
  in
    area_grow_recurrent (remaining_seeds ++ empty_neighbours_indexes) modified_matrix num_columns num_rows

are_empty_fields_creating_single_area:: Matrix Int -> Bool
are_empty_fields_creating_single_area matrix = let
  area_seed = get_index_of_first_empty_field matrix
  matrix_after_area_grow = area_grow area_seed matrix
  as_list = toList matrix_after_area_grow
  in
    (find (==emptyValueField) as_list) == Nothing

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
      | otherwise                   = [(x_i, y_i) | x_i <- [x..(x+1)], y_i <- [y..(y+1)]]


isBoardFilledForIntersection:: Matrix Int -> ((Int, Int), Int) -> Bool
isBoardFilledForIntersection matrix ((x,y), value) =
  let
    indexes_of_neighbouring_fields = getFieldsSurroundingIntersection matrix ((x,y), value)
    neighbouring_values = get_values_under_indexes indexes_of_neighbouring_fields matrix
    num_of_filled_values = length (filter (==filledValueField) neighbouring_values)
  in
    value == num_of_filled_values

isBoardFilledForEveryIntersection :: Matrix Int -> [((Int, Int), Int)] -> Bool
isBoardFilledForEveryIntersection matrix intersections =
  all (==True) (map (\intersection ->  isBoardFilledForIntersection matrix intersection) intersections)

isValidSolution:: Matrix Int -> [((Int, Int), Int)] -> Bool
isValidSolution matrix intersections =
  (isBoardFilledForEveryIntersection matrix intersections) && (are_empty_fields_creating_single_area matrix)


solve:: Matrix Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Maybe(Matrix Int)
solve matrix [] processed_intersections
  | isValidSolution matrix processed_intersections == True  = Just matrix
  | otherwise                                               = Nothing
solve matrix (intersection:intersections) processed_intersections =
  let
    (_, value) = intersection
    fields_around_intersection = getFieldsSurroundingIntersection matrix intersection
    possible_moves = uniqueCombinations value fields_around_intersection
    every_possible_board = map(\moves -> set_values_under_indexes_to_value matrix moves filledValueField) possible_moves

    solve_transform =
      (\transformed_board -> solve transformed_board intersections (intersection:processed_intersections))
    satisfy_condition = (\solution -> not (isNothing solution))
  in
    extractMaybeMatrix (firstSatisfying satisfy_condition (map (solve_transform) every_possible_board))

