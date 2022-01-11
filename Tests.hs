module Tests(
  run_all_tests
) where

import Creek
import Board
import Constants
import Utils

import Data.Matrix

test_get_index_of_first_empty_value:: Bool
test_get_index_of_first_empty_value = let
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
  empty_board = get_empty_board num_rows num_cols

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

test_area_grow_single_channel:: Bool
test_area_grow_single_channel = let
    a = emptyValueField
    b = filledValueField
    c = testValueField

    single_channel_matrix:: Matrix Int
    single_channel_matrix = fromLists [
      [a, a, a, a],
      [b, b, b, a],
      [b, b, a, a],
      [a, a, a, b]
      ]

    expected_output = fromLists [
        [c, c, c, c],
        [b, b, b, c],
        [b, b, c, c],
        [c, c, c, b]
        ]
    output_matrix = area_grow (get_index_of_first_empty_field single_channel_matrix) single_channel_matrix
  in
    output_matrix == expected_output

test_are_empty_fields_creating_single_area_identity_matrix:: Bool
test_are_empty_fields_creating_single_area_identity_matrix = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  identity_matrix = fromLists [
        [b, a, a, a, a],
        [a, b, a, a, a],
        [a, a, b, a, a],
        [a, a, a, b, a],
        [a, a, a, a, b]
        ]
  in
    are_empty_fields_creating_single_area identity_matrix == False

test_are_empty_fields_creating_single_area_single_channel_matrix:: Bool
test_are_empty_fields_creating_single_area_single_channel_matrix = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  single_channel_matrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]
  in
    are_empty_fields_creating_single_area single_channel_matrix == True

test_getFieldsSurroundingIntersection_corners:: ([Bool], Bool)
test_getFieldsSurroundingIntersection_corners = let
  height = 4
  width = 4
  value = 10

  indexes_list = [(i,j)| i <- [1..height], j <- [1..width]]
  indexes_matrix = fromList height width indexes_list

  first_inter:: ((Int, Int), Int)
  first_inter = ((0,0),value)
  first = ((getFieldsSurroundingIntersection indexes_matrix first_inter), [(1,1)])

  second_inter:: ((Int, Int), Int)
  second_inter = ((0,4),value)
  second = ((getFieldsSurroundingIntersection indexes_matrix second_inter), [(1,4)])


  third_inter:: ((Int, Int), Int)
  third_inter = ((4,0),value)
  third = ((getFieldsSurroundingIntersection indexes_matrix third_inter), [(4,1)])

  fourth_inter:: ((Int, Int), Int)
  fourth_inter = ((4,4),value)
  fourth = ((getFieldsSurroundingIntersection indexes_matrix fourth_inter), [(4,4)])

  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth])
    in
      (results, (all (==True) results))

test_getFieldsSurroundingIntersection_edges:: ([Bool], Bool)
test_getFieldsSurroundingIntersection_edges = let
  height = 4
  width = 4
  value = 10

  indexes_list = [(i,j)| i <- [1..height], j <- [1..width]]
  indexes_matrix = fromList height width indexes_list

  first_inter:: ((Int, Int), Int)
  first_inter = ((0,2),value)
  first = ((getFieldsSurroundingIntersection indexes_matrix first_inter), [(1,2), (1,3)])

  second_inter:: ((Int, Int), Int)
  second_inter = ((2, 0),value)
  second = ((getFieldsSurroundingIntersection indexes_matrix second_inter), [(2,1), (3,1)])

  third_inter:: ((Int, Int), Int)
  third_inter = ((4, 1),value)
  third = ((getFieldsSurroundingIntersection indexes_matrix third_inter), [(4,1), (4,2)])

  fourth_inter:: ((Int, Int), Int)
  fourth_inter = ((1, 4),value)
  fourth = ((getFieldsSurroundingIntersection indexes_matrix fourth_inter), [(1,4), (2,4)])

  results = (map tuple_of_list_of_tuples_equal [first, second, third, fourth])
    in
      (results, (all (==True) results))

test_getFieldsSurroundingIntersection_middle:: ([Bool], Bool)
test_getFieldsSurroundingIntersection_middle = let
  height = 4
  width = 4
  value = 10

  indexes_list = [(i,j)| i <- [1..height], j <- [1..width]]
  indexes_matrix = fromList height width indexes_list

  first_inter:: ((Int, Int), Int)
  first_inter = ((1, 1),value)
  first = ((getFieldsSurroundingIntersection indexes_matrix first_inter), [(1,1), (1,2), (2,1), (2,2)])

  second_inter:: ((Int, Int), Int)
  second_inter = ((3, 3),value)
  second = ((getFieldsSurroundingIntersection indexes_matrix second_inter), [(3,3), (3,4), (4,3), (4,4)])

  results = (map tuple_of_list_of_tuples_equal [first, second])
    in
      (results, (all (==True) results))

test_isBoardFilledForIntersection_one_filled:: Bool
test_isBoardFilledForIntersection_one_filled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [b, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((0,0), 1)
  in
    isBoardFilledForIntersection tested_matrix intersection == True

test_isBoardFilledForIntersection_two_filled:: Bool
test_isBoardFilledForIntersection_two_filled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, b, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((0,2), 2)
  in
    isBoardFilledForIntersection tested_matrix intersection == True

test_isBoardFilledForIntersection_three_filled:: Bool
test_isBoardFilledForIntersection_three_filled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, b, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((2,2), 3)
  in
    isBoardFilledForIntersection tested_matrix intersection == True

test_isBoardFilledForIntersection_four_filled:: Bool
test_isBoardFilledForIntersection_four_filled = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, b, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((2,2), 4)
  in
    isBoardFilledForIntersection tested_matrix intersection == True

test_isBoardFilledForEveryIntersection_one_intersection:: Bool
test_isBoardFilledForEveryIntersection_one_intersection = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a, a],
        [a, b, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a],
        [a, a, a, a, a]
        ]

  intersection:: ((Int, Int), Int)
  intersection = ((1,1), 1)
  in
    isBoardFilledForEveryIntersection tested_matrix [intersection] == True

test_isBoardFilledForEveryIntersection_three_intersections:: Bool
test_isBoardFilledForEveryIntersection_three_intersections = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a, a],
        [a, b, b, a, a],
        [a, a, b, a, a],
        [a, a, a, a, a],
        [a, a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1,2), 2), ((2,3),2), ((5,5),1)]
  in
    isBoardFilledForEveryIntersection tested_matrix intersections == True

test_isBoardFilledForEveryIntersection_full_example:: Bool
test_isBoardFilledForEveryIntersection_full_example = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
  in
    isBoardFilledForEveryIntersection tested_matrix intersections == True

test_isValidSolution_full_example::Bool
test_isValidSolution_full_example = let
  a = emptyValueField
  b = filledValueField
  c = testValueField

  tested_matrix = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
  in
    isValidSolution tested_matrix  intersections == True

test_solve_full_example = let

  a = emptyValueField
  b = filledValueField
  c = testValueField

  expected_output = fromLists [
        [a, a, a, a],
        [b, b, b, a],
        [b, b, a, a],
        [a, a, a, b]
        ]

  intersections:: [((Int, Int), Int)]
  intersections = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]

  input_board = get_empty_board 4 4

  Just solution = solve input_board intersections []

  in
    expected_output == solution


run_all_tests:: ([Bool], Bool)
run_all_tests = let
  tests = [
    test_get_index_of_first_empty_value,
    (snd test_get_indexes_of_neighboring_points),
    (snd test_get_indexes_of_empty_neighboring_points_all_empty_fields),
    test_set_values_under_indexes_to_test_value_all_indexes,
    test_set_values_under_indexes_to_test_value_no_indexes,
    test_area_grow_empty_board,
    test_area_grow_split_in_two,
    test_area_grow_identity,
    test_area_grow_single_channel,
    test_are_empty_fields_creating_single_area_identity_matrix,
    test_are_empty_fields_creating_single_area_single_channel_matrix,
    (snd test_getFieldsSurroundingIntersection_corners),
    (snd test_getFieldsSurroundingIntersection_edges),
    (snd test_getFieldsSurroundingIntersection_middle),
    test_isBoardFilledForIntersection_one_filled,
    test_isBoardFilledForIntersection_two_filled,
    test_isBoardFilledForIntersection_three_filled,
    test_isBoardFilledForIntersection_four_filled,
    test_isBoardFilledForEveryIntersection_one_intersection,
    test_isBoardFilledForEveryIntersection_three_intersections,
    test_isBoardFilledForEveryIntersection_full_example,
    test_isValidSolution_full_example,
    test_isValidSolution_full_example,
    test_solve_full_example]
  in
    (tests, (all (==True) tests))
