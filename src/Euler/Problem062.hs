{-# OPTIONS -Wall #-}
module Euler.Problem062
    ( problem062
    ) where

import Data.List (sort, group, elemIndex)

-- problem062
problem062 :: Int
problem062 = fstVal^(3::Integer)

cubeList :: [Integer]
cubeList = [n^(3::Integer) | n <- [0..10000]]

cubeStr :: [String]
cubeStr = map (sort . show) cubeList

fivePerm :: [[String]]
fivePerm = filter ((==5) . length) . group . sort $ cubeStr

fstVal :: Int
Just fstVal = elemIndex (head (head fivePerm)) cubeStr


