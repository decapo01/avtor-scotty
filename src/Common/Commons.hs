module Common.Commons where

import qualified Data.List as List

idxOr :: [a] -> Int -> a -> a
idxOr items idx alt =
    if List.length items  >= (idx + 1)
      then items !! idx
      else alt