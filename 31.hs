{-# LANGUAGE FlexibleInstances #-}

module Euler.ThirtyOne where

  data Q = Q Integer Int deriving Show

  class Monomial a where
     dot :: a -> a -> a
     plus :: a -> a -> a

  instance Monomial Q where
      dot (Q a b) (Q c d) = Q (a * c) (b + d)
      plus (Q a b) (Q c d) = Q (a + c) b

  class Ring a where
     prodotto :: a -> a -> a
     somma :: a -> a -> a

  instance Ring [Q] where
      prodotto a b = flip simplifyPolynomial [] $ concat $ map (\x -> map (dot x) b) a
      somma a b = simplifyPolynomial (a ++ b) []

  simplifyPolynomial :: [Q] -> [Q] -> [Q]
  simplifyPolynomial [] acc             = acc
  simplifyPolynomial (a@(Q _ b):xs) acc = simplifyPolynomial witho (squash : acc)
                                 where withh = filter (\(Q _ c) -> b == c) xs
                                       witho = filter (\(Q _ c) -> b /= c) xs
                                       squash = foldr (plus) a withh

  changeGeneratingFunction :: [[Q]]
  changeGeneratingFunction = [pounds2, pounds1, pence50, pence20, pence10, pence5, pence2, pence1]
                         where pounds2 = [Q 1 0, Q 1 200]
                               pounds1 = [Q 1 0, Q 1 100, Q 1 200]
                               pence50 = map (\i -> Q 1 $ 50*i) [0 .. 4]
                               pence20 = map (\i -> Q 1 $ 20*i) [0 .. 10]
                               pence10 = map (\i -> Q 1 $ 10*i) [0 .. 20]
                               pence5 = map (\i -> Q 1 $ 5*i) [0 .. 40]
                               pence2 = map (\i -> Q 1 $ 2*i) [0 .. 100]
                               pence1 = map (Q 1) [0 .. 200]

  solve = filter (\(Q _ v) -> v == 200) $ powers changeGeneratingFunction
     where powers (x:xs) = foldr (\b a -> filter (\(Q _ c) -> c <= 200) $ prodotto b a) x xs 
