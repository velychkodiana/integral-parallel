module Numerics.SimpsonSeq
  ( simpsonSeq
  ) where

-- | Sequential Simpson's rule for a single refinement step.
--   n must be even and > 0.
simpsonSeq :: (Double -> Double) -> Double -> Double -> Int -> Double
simpsonSeq f a b n
  | n <= 0        = error "simpsonSeq: n must be positive"
  | odd n         = error "simpsonSeq: n must be even"
  | otherwise     =
      let h  = (b - a) / fromIntegral n
          -- odd indices (1,3,5,...,n-1) have weight 4
          s1 = sum [ f (a + fromIntegral (2*k-1) * h) | k <- [1 .. n `div` 2] ]
          -- even indices (2,4,6,...,n-2) have weight 2
          s2 = sum [ f (a + fromIntegral (2*k)   * h) | k <- [1 .. n `div` 2 - 1] ]
      in (h/3) * (f a + f b + 4*s1 + 2*s2)
