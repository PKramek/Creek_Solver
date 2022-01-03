import Creek
import Board
import Constants

import Data.Matrix

--TEST finding indexes in matrix
test_get_index_of_first_empty_or_null_value:: Bool
test_get_index_of_first_empty_or_null_value = let
    num_cols = 20
    num_rows = 20
    test_matrix = fromList num_rows num_cols (repeat testValueField)

    indexes_equal:: ((Int, Int), (Int, Int)) -> Bool
    indexes_equal ((a,b), (c, d)) = a == c && b == d

    indexes:: [((Int, Int), (Int, Int))]
    indexes = [(get_index_of_first_empty_field (setElem emptyValueField (row, col) test_matrix), (row,col)) |
              row <- [1..num_rows], col <- [1..num_cols]]
  in
    all (==True) (map indexes_equal indexes)


--test_get_indexes_of_neighboring_points:: Bool

test_get_indexes_of_neighboring_points:: ([Bool], Bool)
test_get_indexes_of_neighboring_points = let
  num_rows = 5
  num_cols = 5

  tuple_of_list_of_tuples_equal:: ([(Int, Int)], [(Int, Int)]) -> Bool
  tuple_of_list_of_tuples_equal ([],[]) = True
  tuple_of_list_of_tuples_equal ([], y) = False
  tuple_of_list_of_tuples_equal (x, []) = False
  tuple_of_list_of_tuples_equal ((x:xs),(y:ys)) = (fst x) == (fst y) && (snd x) == (snd y) &&
   (tuple_of_list_of_tuples_equal (xs, ys))


--For point (1,1) it should return [(1,2),(2,1)]
  first = ((get_indexes_of_neighboring_points (1,1) num_cols num_rows) , [(1,2), (2,1)])
  --For point (1,5) it should return [(1,2),(2,1)]
  second = ((get_indexes_of_neighboring_points (1,5) num_cols num_rows) , [(1,4), (2,5)])
--For point (5,1) it should return [(5,2), (1,4)]
  third = ((get_indexes_of_neighboring_points (5,1) num_cols num_rows) , [(5,2), (4,1)])
  --For point (5,5) it should return [(5,4), (4,5)]
  fourth = ((get_indexes_of_neighboring_points (5,5) num_cols num_rows) , [(5,4), (4,5)])
  --For point (3,1) it should return [(3,2), (2,1), (4,1)]
  fifth = ((get_indexes_of_neighboring_points (3,1) num_cols num_rows) , [(3,2), (2,1), (4,1)])




  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth, fifth])
  in
    (results, (all (==True) results))

num_rows = 5
num_cols = 5
fifth = ((get_indexes_of_neighboring_points (3,1) num_cols num_rows) , [(3,2), (2,1), (4,1)])


run_all_tests:: ([Bool], Bool)
run_all_tests = let
  tests = [test_get_index_of_first_empty_or_null_value, (snd test_get_indexes_of_neighboring_points)]
  in
    (tests, (all (==True) tests))