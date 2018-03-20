{-# OPTIONS -Wall #-}
module Euler.Problem032
    ( problem032
    ) where

import Data.List (nub, sort, delete, foldl', unfoldr)
import Control.Monad (guard)
-- problem032
-- from https://wiki.haskell.org/Euler_problem/31_to_40
problem032 :: Integer
problem032 = sum pandigiticals

pandigiticals :: [Integer]
pandigiticals = 
  nub 
  $ do 
    (beg,end) <- combs (5::Integer) [1..9]
    n <- [1,2]
    let (a,b) = splitAt n beg
    let res = l2n a * l2n b
    guard $ sort (explode res) == end
    return res

combs :: (Eq t, Eq t1, Num t1) => t1 -> [t] -> [([t], [t])]
combs 0 xs = [([],xs)]
combs n xs = [(y:ys,rest) | y <- xs, (ys,rest) <- combs (n-1) (delete y xs)]

l2n :: [Integer] -> Integer
l2n = foldl' (\a b -> 10*a+b) 0

swap :: (t1, t) -> (t, t1)
swap (a,b) = (b,a)

explode :: Integer -> [Integer]
explode = unfoldr (\a -> if a == 0 then Nothing else Just . swap $ quotRem a 10)


