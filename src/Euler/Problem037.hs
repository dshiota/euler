{-# OPTIONS -Wall #-}
module Euler.Problem037
    ( problem037
    ) where

import Primality as P

-- problem037
problem037 :: Integer
problem037 = sum . take 11 $ filter (\n -> all P.isPrime' (takeLtoR (show n)) && all P.isPrime' (takeRtoL (show n))) $ drop 4 P.primes

takeLtoR :: String -> [Integer]
takeLtoR s = map (\xs -> read xs :: Integer) $ drop 1 $ scanl (\acc c -> acc ++ [c]) "" s

takeRtoL :: String -> [Integer]
takeRtoL s = map (\xs -> read xs :: Integer) $ takeWhile (not .null) $ scanr (:) "" s


