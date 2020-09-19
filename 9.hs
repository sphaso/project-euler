module Euler.Nine where

-- The best optimization we can use is Euclid's formula
-- Since the triplet we're looking for is not primitive, we have to add a `k` parameter which makes the computation a little longer than it should
-- However it's still better than looking for the three parameters individually (even with parity \ divisibility optimizations) since we can reduce the upper bound by quite a lot

  solution :: Int
  solution = head [a*b*c | m<-[2..50], n<-[1..50], k<-[2..50], m > n, let a = k*(m^2-n^2), let b = 2*k*m*n, let c = k*(m^2+n^2), a^2+b^2 == c^2, a+b+c==1000]

