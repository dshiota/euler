module Euler.Problem085
    ( problem085
    ) where

-- https://polyglottal.wordpress.com/2012/09/22/project-euler-problem-85/

-- problem085 :: a
-- problem085 = undefined
problem085 :: Integer
problem085 = a * head b
  where
  (a,b) = head $ filter (not . null . snd) $ map (\k -> (k, bestRects k)) [1..1000]

fact :: (Enum a, Num a) => a -> a
fact n = product [1..n]

c :: Integral a => a -> a -> a
c n r = fact n `div` (fact (n-r) * fact r)

rects :: Integral a => a -> a -> a
rects m n = (m+1) `c` 2 * (n+1) `c` 2

bestRects :: Integral a => a -> [a]
bestRects m = filter (\n -> abs (rects m n - 2000000) < 10) [1..1000]
