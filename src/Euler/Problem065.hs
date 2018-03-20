{-# OPTIONS -Wall #-}
module Euler.Problem065
    ( problem065
    ) where

import Data.Char (digitToInt)
import Data.Ratio (Ratio, (%), numerator)

problem065 :: Int
problem065 = sum $ map digitToInt $ show $ numerator $ fraction $ take 100 eList

eList :: [Integer]
eList = 2: concat [[1,2*k,1]|k<-[1..]]

fraction :: Integral a => [a] -> Ratio a
fraction [x] = x%1
fraction (x:xs) = x%1 + 1 / fraction xs
fraction [] = 0

