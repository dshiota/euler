module Euler.Problem080
    ( problem080
    ) where

problem080 :: Integer
problem080 = sum $ concatMap iSqrt $ filter isNotSquare [2..99]

findX :: (Ord a, Enum a, Num a) => a -> a -> a
findX p c = head [n| n <- [9,8..0], n*(20*p+n) <= c]

iSqrt :: (Num t, Enum t, Ord t) => t -> [t]
iSqrt = iSqrtInner 0 [] 100

iSqrtInner :: (Ord t, Enum t, Num t) => t -> [t] -> Int -> t -> [t]
iSqrtInner p ys len c
  | c == 0 = ys
  | c == 1 = ys ++ [1]
  | length ys == len = ys
  | otherwise = iSqrtInner (p*10+x) (ys++[x]) len ((c-y)*100) 
    where
    x = findX p c
    y = x * (20*p + x)

isNotSquare :: (Enum a, Ord a, Num a) => a -> Bool
isNotSquare n = sq n * sq n /= n
  where sq t = head $ iSqrt t

