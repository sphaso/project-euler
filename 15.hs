module Euler.Fifteen where

  -- (r+s choose s)

  latticePaths :: (Integral a) => a -> a -> a
  latticePaths r s = product [r+1 .. r+s] `div` product [2..s]
