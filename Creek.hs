module Creek(Creek) where
import Utils

data Creek = Creek {
  size:: (Int, Int),
  fields :: [((Int, Int), Int)]
} deriving (Show)

instance Read Creek where
  readsPrec _ input =
    let
--    TODO Refactor this code and ensure that everything works properly
      (creek_str, rest) = splitAt 5 input
      (size_str, fields_str) = splitWhen (=='[') rest
      (height, width) = read size_str :: (Int, Int)
      list_of_moves = read fields_str :: [((Int, Int), Int)]
    in
      [(Creek (height, width) list_of_moves, "")]
