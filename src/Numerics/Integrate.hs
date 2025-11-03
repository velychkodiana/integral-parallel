module Numerics.Integrate
  ( simpsonParallel
  , refineUntilEps
  , chooseFunc
  ) where

import Control.Parallel.Strategies
import Control.DeepSeq (NFData)

-- predefined test functions
chooseFunc :: String -> (Double -> Double)
chooseFunc name =
  case name of
    "poly" -> \x -> x*x + 2*x + 1           -- (x+1)^2
    "sin"  -> sin
    "cos"  -> cos
    "exp"  -> exp
    _      -> error "Unknown --func (use: poly|sin|cos|exp)"

-- parallel summation using chunked evaluation
parSumChunked :: NFData a => Int -> (a -> Double) -> [a] -> Double
parSumChunked chunk f xs =
  let strat = parListChunk chunk rdeepseq
  in  sum $ withStrategy strat (map f xs)

-- Simpson’s rule for numerical integration
simpsonParallel :: Int -> (Double->Double) -> Double -> Double -> Int -> Double
simpsonParallel chunk f a b n
  | n <= 0         = error "n must be positive"
  | odd n          = error "n must be even for Simpson"
  | otherwise      =
      let h   = (b - a) / fromIntegral n
          xs  = [0 .. n]
          step i =
            let x = a + fromIntegral i * h
                w | i==0 || i==n = 1
                  | even i       = 2
                  | otherwise    = 4
            in  fromIntegral w * f x
      in  (h/3) * parSumChunked chunk step xs


simpsonSeq :: (Double -> Double) -> Double -> Double -> Int -> Double
simpsonSeq f a b n =
  let h = (b - a) / fromIntegral n
      s1 = sum [f (a + fromIntegral (2*k-1) * h) | k <- [1 .. n `div` 2]]
      s2 = sum [f (a + fromIntegral (2*k)   * h) | k <- [1 .. n `div` 2 - 1]]
  in (h/3) * (f a + f b + 4*s1 + 2*s2)

-- adaptive refinement based on accuracy ε
refineUntilEps :: Int -> (Double->Double) -> Double -> Double -> Double
               -> Int -> Int -> (Double, Int)
refineUntilEps chunk f a b eps n0 nMax =
  go n0 (simpsonParallel chunk f a b (makeEven n0))
  where
    makeEven m = if even m then m else m + 1
    go n prev
      | n >= nMax = (prev, n)
      | otherwise =
          let n'   = makeEven (2*n)
              cur  = simpsonParallel chunk f a b n'
          in  if abs (cur - prev) < eps
                 then (cur, n')
                 else go n' cur
