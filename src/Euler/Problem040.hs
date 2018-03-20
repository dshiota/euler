{-# OPTIONS -Wall #-}
module Euler.Problem040
    ( problem040
    ) where

-- problem040
problem040 :: Integer
problem040 = product $ map (\n -> read [fracList !! n] :: Integer) [0, 9, 99, 999, 9999, 99999, 999999]
fracList :: String
fracList = concatMap show [(1::Integer)..]


