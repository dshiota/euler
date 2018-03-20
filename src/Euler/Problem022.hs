{-# OPTIONS -Wall #-}
module Euler.Problem022
    ( problem022
    ) where

import Data.List (sort)
import Data.Char (ord)

-- problem022
problem022 :: IO ()
problem022 = do
  contents <- readFile "../p022_names.txt"
  let names = sort (read ("[" ++ contents ++ "]"))
  let scores = zipWith score names [1..]
  print (sum scores)

score :: String -> Int -> Int
score name rank = (rank * ) (sum (map (\c -> ord c - ord 'A' + 1) name))


