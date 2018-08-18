module Euler.One where

  solution :: (RealFrac a, Integral b) => a -> b
  solution n = multiplesSummer n 3 + multiplesSummer n 5 - multiplesSummer n 15

  floorDiv :: (RealFrac a) => a -> a -> a
  floorDiv n d = fromIntegral $ floor $ n / d

  summer :: (RealFrac a, Integral b) => a -> a -> b
  summer x d = floor $ x * d * (d + 1) / 2

  multiplesSummer :: (RealFrac a, Integral b) => a -> a -> b
  multiplesSummer n d = let a = floorDiv n d in summer d a
