{-# OPTIONS -Wall #-}
module Euler.Problem009
    ( problem009
    ) where

-- problem9
problem009 :: Integer
problem009 = product (head [[a,b,c] | a<-[1..998], b<-[(a+1)..999], c<-[(b+1)..(1000-(a+b))], a^(2::Integer)+b^(2::Integer) == c^(2::Integer), a+b+c==1000])

