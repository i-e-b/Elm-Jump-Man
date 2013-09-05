module Sort (quicksort, sortReverse) where

quicksort list =
  case list of
    [] -> []
    pivot::rest ->
        let lower  = filter (\n -> n <= pivot) rest
            higher = filter (\n -> n >  pivot) rest
        in  quicksort lower ++ [pivot] ++ quicksort higher

sortReverse list = case list of
    [] -> []
    pivot::rest ->
        let lower = filter (\n -> n < pivot) rest
            higher= filter (\n -> n >=  pivot) rest
        in  quicksort higher ++ [pivot] ++ quicksort lower
