import Creek
import Board
import Constants
import Utils

import Data.Matrix

test_get_index_of_first_empty_or_null_value:: Bool
test_get_index_of_first_empty_or_null_value = let
    num_cols = 20
    num_rows = 20
    test_matrix = fromList num_rows num_cols (repeat testValueField)

    indexes:: [((Int, Int), (Int, Int))]
    indexes = [(get_index_of_first_empty_field (setElem emptyValueField (row, col) test_matrix), (row,col)) |
              row <- [1..num_rows], col <- [1..num_cols]]
  in
    all (==True) (map indexes_equal indexes)

test_get_indexes_of_neighboring_points:: ([Bool], Bool)
test_get_indexes_of_neighboring_points = let
  num_rows = 5
  num_cols = 5

  --For point (1,1) it should return [(1,2),(2,1)]
  first = ((get_indexes_of_neighboring_points (1,1) num_cols num_rows), [(1,2), (2,1)])
  --For point (1,5) it should return [(1,2),(2,1)]
  second = ((get_indexes_of_neighboring_points (1,5) num_cols num_rows), [(1,4), (2,5)])
  --For point (5,1) it should return [(5,2), (1,4)]
  third = ((get_indexes_of_neighboring_points (5,1) num_cols num_rows), [(5,2), (4,1)])
  --For point (5,5) it should return [(5,4), (4,5)]
  fourth = ((get_indexes_of_neighboring_points (5,5) num_cols num_rows), [(5,4), (4,5)])
  --For point (3,1) it should return [(3,2), (2,1), (4,1)]
  fifth = ((get_indexes_of_neighboring_points (3,1) num_cols num_rows), [(3,2), (2,1), (4,1)])
  --For point (1, 3) it should return [(1,2), (1,4), (2,3)]
  sixth = ((get_indexes_of_neighboring_points (1,3) num_cols num_rows), [(1,2), (1,4), (2,3)])
  --For point (3, 3) it should return [(3,2), (3,4), (2,3), (4,3)]
  seventh = ((get_indexes_of_neighboring_points (3,3) num_cols num_rows), [(3,2), (3,4), (2,3), (4,3)])

  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth, fifth, sixth, seventh])
  in
    (results, (all (==True) results))


test_get_indexes_of_empty_neighboring_points_all_empty_fields::([Bool], Bool)
test_get_indexes_of_empty_neighboring_points_all_empty_fields = let
  num_rows = 5
  num_cols = 5
  empty_board:: Matrix Int
  empty_board = change_all_null_values_to_empty (get_empty_board num_rows num_cols)

--  For point (1,1) it should return [(1,2),(2,1)]
  first = ((get_indexes_of_empty_neighboring_points (1,1) empty_board num_cols num_rows), [(1,2), (2,1)])
  --For point (1,5) it should return [(1,2),(2,1)]
  second = ((get_indexes_of_empty_neighboring_points (1,5) empty_board num_cols num_rows), [(1,4), (2,5)])
  --For point (5,1) it should return [(5,2), (1,4)]
  third = ((get_indexes_of_empty_neighboring_points (5,1) empty_board num_cols num_rows), [(5,2), (4,1)])
  --For point (5,5) it should return [(5,4), (4,5)]
  fourth = ((get_indexes_of_empty_neighboring_points (5,5) empty_board num_cols num_rows), [(5,4), (4,5)])
  --For point (3,1) it should return [(3,2), (2,1), (4,1)]
  fifth = ((get_indexes_of_empty_neighboring_points (3,1) empty_board num_cols num_rows), [(3,2), (2,1), (4,1)])
  --For point (1, 3) it should return [(1,2), (1,4), (2,3)]
  sixth = ((get_indexes_of_empty_neighboring_points (1,3) empty_board num_cols num_rows), [(1,2), (1,4), (2,3)])
  --For point (3, 3) it should return [(3,2), (3,4), (2,3), (4,3)]
  seventh = ((get_indexes_of_empty_neighboring_points (3,3) empty_board num_cols num_rows), [(3,2), (3,4), (2,3), (4,3)])

  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth, fifth, sixth, seventh])
  in
    (results, (all (==True) results))

test_get_indexes_of_empty_neighboring_points_just_nones::([Bool], Bool)
test_get_indexes_of_empty_neighboring_points_just_nones = let
  num_rows = 5
  num_cols = 5
  empty_board:: Matrix Int
  empty_board = get_empty_board num_rows num_cols

  --  Empty list should always be returned
  first = ((get_indexes_of_empty_neighboring_points (1,1) empty_board num_cols num_rows), [])
  second = ((get_indexes_of_empty_neighboring_points (1,5) empty_board num_cols num_rows), [])
  third = ((get_indexes_of_empty_neighboring_points (5,1) empty_board num_cols num_rows), [])
  fourth = ((get_indexes_of_empty_neighboring_points (5,5) empty_board num_cols num_rows), [])
  fifth = ((get_indexes_of_empty_neighboring_points (3,1) empty_board num_cols num_rows), [])
  sixth = ((get_indexes_of_empty_neighboring_points (1,3) empty_board num_cols num_rows), [])
  seventh = ((get_indexes_of_empty_neighboring_points (3,3) empty_board num_cols num_rows), [])

  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth, fifth, sixth, seventh])
  in
    (results, (all (==True) results))

test_set_values_under_indexes_to_test_value_all_indexes::Bool
test_set_values_under_indexes_to_test_value_all_indexes = let
    num_rows = 5
    num_cols = 5
    empty_board:: Matrix Int
    empty_board = get_empty_board num_rows num_cols

    matrix_with_just_test_value = fromList num_rows num_cols (repeat testValueField)

    all_indexes = [(row, col) | row <- [1..num_rows], col <-[1..num_cols]]

    output_matrix = set_values_under_indexes_to_value empty_board all_indexes testValueField

    in
      matrix_with_just_test_value == output_matrix

test_set_values_under_indexes_to_test_value_no_indexes::Bool
test_set_values_under_indexes_to_test_value_no_indexes = let
    num_rows = 5
    num_cols = 5
    empty_board:: Matrix Int
    empty_board = get_empty_board num_rows num_cols

    output_matrix = set_values_under_indexes_to_value empty_board [] testValueField

    in
      empty_board == output_matrix

test_area_grow_empty_board:: Bool
test_area_grow_empty_board = let
  num_rows = 5
  num_cols = 5

  all_indexes = [(row, col) | row <- [1..num_rows], col <-[1..num_cols]]

  empty_board:: Matrix Int
  empty_board = set_values_under_indexes_to_value (get_empty_board num_rows num_cols) all_indexes testValueField
  matrix_with_just_test_value = fromList num_rows num_cols (repeat testValueField)
  output_matrix = area_grow (1, 1) empty_board
  in
    output_matrix == matrix_with_just_test_value

test_area_grow_split_in_two:: Bool
test_area_grow_split_in_two = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    split_in_two_matrix:: Matrix Int
    split_in_two_matrix = fromLists (take 5 (repeat [a,a,b,a,a]))
    expected_output:: Matrix Int
    expected_output = fromLists (take 5 (repeat [c,c,b,a,a]))

    output_matrix = area_grow (1, 1) split_in_two_matrix
  in
    output_matrix == expected_output


test_area_grow_identity:: Bool
test_area_grow_identity = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    identity_matrix:: Matrix Int
    identity_matrix = fromLists [
      [b, a, a, a, a],
      [a, b, a, a, a],
      [a, a, b, a, a],
      [a, a, a, b, a],
      [a, a, a, a, b]
      ]
    expected_output:: Matrix Int
    expected_output = fromLists [
      [b, c, c, c, c],
      [a, b, c, c, c],
      [a, a, b, c, c],
      [a, a, a, b, c],
      [a, a, a, a, b]
      ]

    output_matrix = area_grow (get_index_of_first_empty_field identity_matrix) identity_matrix
  in
    output_matrix == expected_output



run_all_tests:: ([Bool], Bool)
run_all_tests = let
  tests = [test_get_index_of_first_empty_or_null_value,
   (snd test_get_indexes_of_neighboring_points),
   (snd test_get_indexes_of_empty_neighboring_points_all_empty_fields),
   (snd test_get_indexes_of_empty_neighboring_points_just_nones),
   test_set_values_under_indexes_to_test_value_all_indexes,
   test_set_values_under_indexes_to_test_value_no_indexes,
   test_area_grow_empty_board,
   test_area_grow_split_in_two,
   test_area_grow_identity
   ]
  in
    (tests, (all (==True) tests))
