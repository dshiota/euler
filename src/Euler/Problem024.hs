{-# OPTIONS -Wall #-}
module Euler.Problem024
    ( problem024
    ) where

import Data.List (sort, permutations)

-- problem024
problem024 :: String
problem024 = (sort . permutations $ "0123456789") !! 999999


