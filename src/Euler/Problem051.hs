{-# OPTIONS -Wall #-}
module Euler.Problem051
    ( problem051
    ) where

import Primality as P

-- problem051
problem051 :: Integer
problem051 = head [n | 
  n <- [100003,100005..999999],
  numChar n == (3::Integer),
  sum (same n) == (8::Integer)
  ]
    

ch :: Char
ch='1'

numChar :: (Show a1, Num a) => a1 -> a
numChar n = sum [1 | x<-show n, x==ch]

replace :: Char -> Char -> Char
replace d c
  | c == ch = d
  | otherwise = c

nextN :: Show a => (Char -> Char) -> a -> Integer
nextN repl n = (+(0::Integer)) $ read $ map repl $ show n

same :: (Num t, Show a1) => a1 -> [t]
same n = [if P.isPrime $ nextN (replace a) n then 1 else 0 | a <- ['1'..'9']]


