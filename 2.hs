module Euler.Two where

  solve :: Int
  solve = sum $ takeWhile (<4000000) evenFibs 

  evenFibs :: [Int]
  evenFibs = 2 : 8 : zipWith (\a b -> 4 * b + a) evenFibs (tail evenFibs)

