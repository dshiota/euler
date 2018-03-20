{-# OPTIONS -Wall #-}
module Euler.Problem059
    ( problem059
    ) where

import Data.Char (ord, chr)
import Data.Bits (xor)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- problem059
-- https://wiki.haskell.org/Euler_problems/51_to_60

keys :: [[Int]]
keys = [ [a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122] ]
allAlpha :: String -> Bool
allAlpha = all (\k -> let a = ord k in (a >= 32 && a <= 122))
howManySpaces :: String -> Int
howManySpaces = length . filter (==' ')

problem059 :: IO ()
problem059 = do
  s <- readFile "p059_cipher.txt"
  let
    cipher = read ("[" ++ s ++ "]") :: [Int]
    decrypts = [ map chr (zipWith xor (cycle key) cipher) | key <- keys]
    alphaDecrypts = filter allAlpha decrypts
    message = maximumBy (comparing howManySpaces) alphaDecrypts
    asciisum = sum (map ord message)
  print asciisum


