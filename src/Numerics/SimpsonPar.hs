module Numerics.SimpsonPar
  ( simpsonPar
  ) where

import Control.Parallel.Strategies (withStrategy, parListChunk, rdeepseq)
import Control.DeepSeq (NFData)

-- | Parallel Simpson's rule using chunked evaluation.
--   'chunk' controls granularity of parallel tasks.
simpsonPar :: Int -> (Double -> Double) -> Double -> Double -> Int -> Double
simpsonPar chunk f a b n
  | n <= 0        = error "simpsonPar: n must be positive"
  | odd n         = error "simpsonPar: n must be even"
  | otherwise     =
      let h  = (b - a) / fromIntegral n
          is = [0 .. n]
          step i =
            let x = a + fromIntegral i * h
                w | i==0 || i==n = 1
                  | even i       = 2
                  | otherwise    = 4
            in fromIntegral w * f x
          strat = parListChunk (max 1 chunk) rdeepseq
      in (h/3) * sum (withStrategy strat (map step is))
