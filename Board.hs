module Board(get_empty_board, change_all_null_values_to_empty, get_index_of_first_empty_field,
 get_indexes_of_neighboring_points) where

import Data.Matrix
import Data.List
import Constants

get_empty_board:: Int -> Int -> Matrix Int
get_empty_board height width = matrix height width $ \(i,j) -> nullValueField

change_all_null_values_to_empty:: Matrix Int -> Matrix Int
change_all_null_values_to_empty matrix = (mapPos (\(r,c) a -> if a == nullValueField then emptyValueField else a) matrix)

getIndexFromMaybe:: Maybe a -> a
getIndexFromMaybe Nothing = error "Index not found"
getIndexFromMaybe (Just index) = index

get_index_of_first_empty_field:: Matrix Int -> (Int, Int)
get_index_of_first_empty_field matrix =let
    num_columns = fromIntegral (ncols matrix)
    num_rows = fromIntegral (nrows matrix)
    not_null_matrix = change_all_null_values_to_empty matrix
    matrix_as_list = toList not_null_matrix
    index_in_list = fromIntegral (getIndexFromMaybe (elemIndex emptyValueField matrix_as_list))

    row = index_in_list `div` num_rows
    column = index_in_list `mod` num_columns
  in
--   One is added because in Haskell matrices are indexed from 1
    (row + 1, column + 1)

get_indexes_of_neighboring_points:: (Int, Int) -> Int -> Int -> [(Int, Int)]
get_indexes_of_neighboring_points (row, col) num_columns num_rows =
  let
    connected_points = [(row, col-1), (row, col+1), (row-1, col), (row+1, col)]
  in
    filter (\(c, r) -> c >= 1 && r >= 1 && c <= num_columns && r <= num_rows) connected_points



