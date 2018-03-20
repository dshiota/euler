{-# OPTIONS -Wall #-}
module Euler.Problem038
    ( problem038
    ) where

import Data.List (sort)

-- problem038
problem038 :: Integer
problem038 = maximum $ map (\s -> read s :: Integer) $ filter isPandigital9 $ map take9digits [(1::Integer)..9999]

take9digits :: (Enum a, Num a, Show a) => a -> String
take9digits n = take 9 $ concatMap (show . (* n)) [1,2..]

isPandigital9 :: String -> Bool
isPandigital9 xs = sort xs == "123456789"


