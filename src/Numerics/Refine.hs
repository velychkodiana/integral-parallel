module Numerics.Refine
  ( Integrator
  , refineUntilEps
  ) where

-- | A single-step integrator type:
--   (f a b n) -> approximate integral using exactly n subintervals (n even).
type Integrator = (Double -> Double) -> Double -> Double -> Int -> Double

-- | Binary refinement: repeatedly double n until the solution stabilizes
--   to within eps, or until n exceeds nMax. Returns (value, nUsed).
refineUntilEps
  :: Integrator
  -> (Double -> Double)   -- f
  -> Double               -- a
  -> Double               -- b
  -> Double               -- eps
  -> Int                  -- n0 (initial, will be rounded up to even)
  -> Int                  -- nMax (cap)
  -> (Double, Int)
refineUntilEps integrate f a b eps n0 nMax =
  let evenN m = if even m then m else m + 1
      go n prev
        | n >= nMax  = (prev, n)
        | otherwise  =
            let n'  = evenN (2*n)
                cur = integrate f a b n'
            in if abs (cur - prev) < eps
                  then (cur, n')
                  else go n' cur
      n0e = evenN (max 2 n0)
      first = integrate f a b n0e
  in go n0e first
