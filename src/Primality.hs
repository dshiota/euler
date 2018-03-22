{-# OPTIONS -Wall #-}
module Primality
    ( factors
    , isPrime
    , isPrime'
    , pfactors
    , primes
    , primeFactors
    , millerRabinPrimality
    , factorize
    , factorial
    ) where

import Data.List (group, unfoldr)
import Data.Maybe

factors :: Integral t => t -> [t]
factors n
  = unfoldr (\(d,n') -> listToMaybe [(x, (x, div n' x)) | x <- takeWhile ((<=n').(^(2::Integer)))
                                                  [d..] ++ [n'|n'>1], mod n' x == 0]) (2,n)
 
isPrime :: Integer -> Bool
isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

isPrime' :: Integer -> Bool
isPrime' 1 = False
isPrime' n = 1 == (snd . head $ xs) && 1 == length xs
  where
    xs = factorize n

pfactors :: Integral a => [a] -> a -> [a]
pfactors prs n = unfoldr (\(ds, n') -> listToMaybe
  [(x, (dropWhile (< x) ds, div n' x)) | x <- takeWhile ((<=n').(^(2::Integer)))
                                                ds ++ [n'|n'>1], mod n' x == 0]) (prs, n)

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n | n > 1 = go n primes
  where
    -- go _ [] = 1
    go n' ps@(p:t)
      | p*p > n'   = [n']
      | r == 0    = p : go q ps
      | otherwise =     go n' t
          where
            (q,r) = quotRem n' p
primeFactors _ = [1]

-- (eq. to) find2km (2^k * n) = (k, n)
find2km :: Integral a => a -> (a, a)
find2km = f 0
  where
    f k m
      | r == 1 = (k, m)
      | otherwise = f (k+1) q
      where (q,r) = quotRem m 2

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
  | a <= 1 || a >= n-1 = 
    error $ "millerRabinPrimality: a out of range ("
          ++ show a ++ " for " ++ show n ++ ")"
  | n < 2 = False
  | even n = False
  | b0 == 1 || b0 == n' = True
  | otherwise = iter (tail b)
  where
    n' = n-1
    (k,m) = find2km n'
    b0 = powMod n a m
    b = take (fromIntegral k) $ iterate (squareMod n) b0
    iter [] = False
    iter (x:xs)
      | x == 1 = False
      | x == n' = True
      | otherwise = iter xs

-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a -> a -> a) -> (a -> a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
  where
    f x n y
      | n == 1 = x `mul` y
      | r == 0 = f x2 q y
      | otherwise = f x2 q (x `mul` y)
      where
        (q,r) = quotRem n 2
        x2 = sq x

mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a

squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

factorize :: Integer -> [(Integer,Integer)]
factorize 1 = [(1,0::Integer)] -- 1^0
factorize n = format $ factorize' n primes
  where
    factorize' _ [] = undefined
    factorize' n' ps@(p:ps')
      | p * p > n' = [n']
      | rem n' p == 0 = p : factorize' (div n' p) ps
      | otherwise = factorize' n' ps'
    format ps = [(x, toInteger $ length xs) | xs@(x:_) <- group ps]

factorial :: (Num t, Ord t) => t -> t
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n-1)
 
