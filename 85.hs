module Euler.EigthyFive where

-- There's a combinatorial interpretation for which
-- the number of rectangles given a m*n rectangle is
-- C(m+1, 2) * C(n+1, 2)
-- That is, the number of ways of choosing 2 vertical sides
-- and two horizontal sides (+1 to account for the boundary).

-- One equation... two variables, what gives?
-- Massaging the binomials we have:
-- (m*(m+1)*n*(n+1)) / 4 ~ 2kk
-- We can check for mod 4 and reduce the search space significantly
-- To simplify computation, we see that this equals another easier condition:
-- either m or n need to be even
--
-- The bound of 2000 is found supposing n = 1 and solving the quadratic equation

   equation :: Int -> Int -> Int
   equation m n = (m ^ 2 + m) * (n ^ 2 + n)

   fullEquation :: Int -> Int -> Int
   fullEquation m n = (flip div 4) $ equation m n

   search :: [(Int, Int)]
   search = filter (\(a, _) -> a < 2000000 && a > 1999900) [(fullEquation m n, m * n) | m <- reverse [1 .. 2000], n <- [1 .. 1000], m `mod` 2 == 0 || n `mod` 2 == 0, m > n] 
