module Euler.Six where

squareSum :: Int -> Int
squareSum n = (n*(n+1) `div` 2) ^ 2

sumSquare :: Int -> Int
sumSquare n = n*(n+1)*(2*n+1) `div` 6

solve :: Int -> Int
solve n = (squareSum n) - (sumSquare n)
