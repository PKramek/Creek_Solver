module Creek(Creek) where
import Utils

data Creek = Creek {
  size:: (Int, Int),
  fields :: [((Int, Int), Int)]
} deriving (Show)

instance Read Creek where
  readsPrec _ input =
    let
      (creek_str, rest) = splitWhen (==' ') input
      (size_str, fields_str) = splitWhen (=='[') rest
      (height, width) = read size_str :: (Int, Int)
      list_of_moves = read fields_str :: [((Int, Int), Int)]

    in
      if creek_str /= "Creek"
        then error "Input string should start with Creek"
      else
        [(Creek (height, width) list_of_moves, "")]


