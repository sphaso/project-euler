module Euler.TwentyEight where

  oddNumbers :: (Integral a) => [a]
  oddNumbers = 3 : map (+2) oddNumbers

  evens :: (Integral a) => [a]
  evens = map ((-) 1) oddNumbers 

  mainDiag :: (Integral b) => [(b, b)]
  mainDiag = zip (map (^2) oddNumbers) evens

  solve :: (RealFrac a, Integral b) => a -> b 
  solve n = foldr (\(p, l) a -> a + p + p - l + p - 2*l + p - 3*l) 1 path
        where path = take (floor $ n / 2) mainDiag 
