{-# OPTIONS -Wall #-}
module Euler.Problem056
    ( problem056
    ) where

-- problem056
problem056 :: Int
problem056 = maximum $ map digitSum [a^b | a <- [(1::Integer)..99], b <- [(1::Integer)..99]]

digitSum :: Show a => a -> Int
digitSum n = sum $ map (\c -> read [c] :: Int) $ show n


