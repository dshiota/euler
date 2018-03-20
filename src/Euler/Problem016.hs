{-# OPTIONS -Wall #-}
module Euler.Problem016
    ( problem016
    ) where

-- problem016 2^1000
problem016 :: Show a => a -> Integer
problem016 = sumStringNum

sumStringNum :: Show a => a -> Integer
sumStringNum n = sum ( map (\c -> read [c] :: Integer) (show n)) 


