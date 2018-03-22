{-# OPTIONS -Wall #-}
module EulerUtil
    ( isPalindrome
    ) where

isPalindrome :: Show a => a -> Bool
isPalindrome n = show n == reverse (show n)

