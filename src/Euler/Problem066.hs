{-# OPTIONS -Wall #-}
module Euler.Problem066
    ( problem066
    ) where

-- https://wiki.haskell.org/Euler_problems/61_to_70
problem066 :: Integer
problem066 = snd $ maximum 
  [(x,d) | d <- [1..1000],
  let b = intSqrt d,
  b*b /= d,
  let (x,_) = pell d b b
  ]

intSqrt :: Integral t => t -> t
intSqrt n
  | n < 0 = error "intSqrt: negative n"
  | otherwise = f n
  where
    f x | y < x = f y
        | otherwise = x
        where y = (x + (n `quot` x)) `quot` 2

pell :: Integral t => t -> t -> t -> (t, t)
pell d wd b = piter d wd b (0::Int) 1 0 1 1 0

piter :: (Integral t, Integral t1) => t1 -> t1 -> t1 -> t -> t1 -> t1 -> t1 -> t1 -> t1 -> (t1, t1)
piter d wd b i c l k m n
  | cn == 1 = (x, y)
  | otherwise = piter d wd bn (i+1) cn k u n v
  where
  yb = (wd+b) `div` c
  bn = yb*c-b
  cn = (d-(bn*bn)) `div` c
  yn | i == 0 = wd
     | otherwise = yb
  u = k*yn+l
  v = n*yn+m
  (x, y) | odd (i+1) = (u*u+d*v*v, 2*u*v)
         | otherwise = (u,v)
