{-# OPTIONS -Wall #-}
module Euler.Problem017
    ( problem017
    ) where

-- problem017
problem017 :: Int
problem017 = sum $ map (length . numString) [1..1000]

numString :: Int -> String
numString n
  | n < 20 = numStr0 !! n
  | n < 100 = (numStrTy !! (n `div` 10)) ++ numString (n `mod` 10)
  | n < 1000 && n `mod` 100 == 0 = (numStr0 !! (n `div` 100)) ++ "hundred"
  | n < 1000 = (numStr0 !! (n `div` 100)) ++ "hundredand" ++
               numString (n `mod` 100)
  | n == 1000 = "onethousand"
  | otherwise = ""

numStr0 :: [String]
numStr0 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

numStrTy :: [String]
numStrTy = ["", "", "twenty", "thirty", "forty", "fifty",
            "sixty", "seventy", "eighty", "ninety"]
{-
one = ["one","two","three","four","five","six","seven","eight",
     "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
          "sixteen","seventeen","eighteen", "nineteen"]
ty = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
           
decompose x 
  | x == 0                       = []
  | x < 20                       = one !! (x-1)
  | x >= 20 && x < 100           = 
    ty !! (firstDigit (x) - 2) ++ decompose ( x - firstDigit (x) * 10)
  | x < 1000 && x `mod` 100 ==0  = 
    one !! (firstDigit (x)-1) ++ "hundred"
  | x > 100 && x <= 999          = 
    one !! (firstDigit (x)-1) ++ "hundredand" ++decompose ( x - firstDigit (x) * 100)
  | x == 1000                    = "onethousand"
    where firstDigit x = digitToInt . head . show $ x

problem_17 = length . concatMap decompose $ [1..1000]
-}

