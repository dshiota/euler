module Euler.Problem083
    ( problem083
    ) where

import Data.Graph.Inductive (Gr,mkGraph,spTree)
import Data.Graph.Inductive.Graph ()
import Data.Graph.Inductive.Query.SP ()
import Data.Graph.Inductive.Internal.RootPath (getDistance)
import Data.List (unfoldr, minimumBy)
import Data.Ord (comparing)
import Control.Arrow ((***))

-- https://wiki.haskell.org/Euler_problems/81_to_90
type Matrix = [[Int]]
type IJ = (Int, Int)

connect83 :: [IJ]
connect83 = [(-1,0),(0,-1),(1,0),(0,1)]

dimensions :: Matrix -> IJ
dimensions matrix = (length matrix, length (head matrix))

ijToindex :: Matrix -> IJ -> Int
ijToindex matrix (i,j) = i*rows + j
  where (rows,_) = dimensions matrix

{-
indexToij :: Matrix -> Int -> IJ
indexToij matrix index = divMod index rows
  where (rows,_) = dimensions matrix
-}

ijValid :: Matrix -> [IJ] -> [IJ]
ijValid matrix = filter f
  where (rows,cols) = dimensions matrix
        f (i,j) = i >= 0 && i < rows && j >= 0 && j < cols

ijPlus :: IJ -> IJ -> IJ
ijPlus (i1,j1) (i2,j2) = (i1+i2,j1+j2)

mEdges :: Matrix -> [IJ] -> IJ -> [(Int, Int, Int)]
mEdges matrix connectL (i,j) = 
  let ijs = ijValid matrix $ map (ijPlus (i,j)) connectL
  in map (\(x,y) -> (ijToindex matrix (i,j),
                     ijToindex matrix (x,y),
                     matrix !! x !! y)) ijs

mGraph :: Matrix -> [IJ] -> Gr IJ Int
mGraph matrix connectL = 
  let (rows,cols) = dimensions matrix
      ijs = [(i,j) | i <- [0..(rows-1)], j <- [0..(cols-1)]]
      mnodes = map (\(x,y) -> (ijToindex matrix (x,y), (x,y))) ijs
      medges = concatMap (mEdges matrix connectL) ijs
  in mkGraph mnodes medges

mSPlen :: Matrix -> [IJ] -> [IJ] -> [IJ] -> ((IJ, IJ), Maybe Int)
mSPlen matrix connectL from to = 
  let -- (rows,cols) = dimensions matrix
      mx (i,j) = matrix !! i !! j
      ijI = ijToindex matrix
      gr = mGraph matrix connectL
      spTrees = [(x,spTree (ijI x) gr) | x <- from]
      distance (i,j) = getDistance (ijI (i,j))
      distances = [((a,y), (+ mx a) <$> distance y b) | (a,b) <- spTrees, y <- to]
  in minimumBy (comparing snd) distances

mName :: String
mName = "Euler/p081_matrix.txt"

columns :: String  -> [Int]
columns = 
  unfoldr f
  where
  f [] = Nothing
  f xs = Just $ (read *** drop 1) $ break (==',') xs

problem083 :: IO()
problem083 = do
  f <- readFile mName
  let matrix = map columns $ lines f
      (rows,cols) = dimensions matrix
      -- firstColumn = [(i,0)      | i <- [0..(rows-1)]]
      -- lastColumn  = [(i,rows-1) | i <- [0..(rows-1)]]
      topLeft = [(0,0)]
      bottomRight = [(rows-1,cols-1)]
  print (mSPlen matrix connect83 topLeft bottomRight)

