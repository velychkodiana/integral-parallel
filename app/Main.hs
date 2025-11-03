module Main where

import System.Environment (getArgs)
import System.Exit        (exitSuccess)
import System.CPUTime     (getCPUTime)
import Text.Printf        (printf)
import GHC.Conc           (setNumCapabilities)
import Control.DeepSeq    (NFData, deepseq)

import Numerics.Funcs
import Numerics.SimpsonSeq
import Numerics.SimpsonPar
import Numerics.Refine

-- -------- Command-line args (simple manual parsing) --------

data Args = Args
  { a        :: Double
  , b        :: Double
  , eps      :: Double
  , threads  :: Int
  , func     :: String
  , n0       :: Int
  , nMax     :: Int
  , chunk    :: Int
  , compareM :: Bool
  } deriving Show

defaults :: Args
defaults = Args
  { a        = 0
  , b        = 1
  , eps      = 1e-8
  , threads  = 0
  , func     = "sin"
  , n0       = 1024
  , nMax     = 8 * 1024 * 1024
  , chunk    = 1024
  , compareM = false
  }

false :: Bool
false = False

parseArgs :: [String] -> Args
parseArgs = go defaults
  where
    go acc []                     = acc
    go acc ("--a":x:xs)           = go acc{a=read x} xs
    go acc ("--b":x:xs)           = go acc{b=read x} xs
    go acc ("--eps":x:xs)         = go acc{eps=read x} xs
    go acc ("--threads":x:xs)     = go acc{threads=read x} xs
    go acc ("--func":x:xs)        = go acc{func=x} xs
    go acc ("--n0":x:xs)          = go acc{n0=read x} xs
    go acc ("--nmax":x:xs)        = go acc{nMax=read x} xs
    go acc ("--chunk":x:xs)       = go acc{chunk=read x} xs
    go acc ("--compare":xs)       = go acc{compareM=True} xs
    go _   (bad:_)                = error $ "Unknown arg: " ++ bad

-- -------- timing helper --------

timePure :: NFData a => a -> IO (a, Double)
timePure x = do
  t0 <- getCPUTime
  x `deepseq` pure ()
  t1 <- getCPUTime
  let sec = fromIntegral (t1 - t0) / 1.0e12
  pure (x, sec)

-- -------- main --------

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  -- Bind RTS capabilities if requested.
  if threads args > 0 then setNumCapabilities (threads args) else pure ()

  let f        = chooseFunc (func args)
      a'       = a args
      b'       = b args
      eps'     = eps args
      n0'      = n0 args
      nMax'    = nMax args
      chunk'   = max 1 (chunk args)

  putStrLn "Parallel Simpson Integrator (seq vs par, ε-refinement)"
  putStrLn $ "  func   = " ++ func args ++ "  (available: " ++ show availableFuncs ++ ")"
  putStrLn $ "  [a,b]  = [" ++ show a' ++ ", " ++ show b' ++ "]"
  putStrLn $ "  eps    = " ++ show eps'
  putStrLn $ "  n0     = " ++ show n0' ++ ", nMax = " ++ show nMax'
  putStrLn $ "  chunk  = " ++ show chunk'
  putStrLn $ "  threads= " ++ show (threads args) ++ "  (use +RTS -N<k> -s)"

  -- Optional: simple seq vs par comparison at fixed n (n0)
  if compareM args
    then do
      putStrLn "\n== Compare at fixed n (no refinement) =="
      let nTest = if even n0' then n0' else n0'+1
      (rSeq, tSeq) <- timePure (simpsonSeq f a' b' nTest)
      (rPar, tPar) <- timePure (simpsonPar chunk' f a' b' nTest)
      printf "Sequential:  result = %.12f   time = %.6f s\n" rSeq tSeq
      printf "Parallel:    result = %.12f   time = %.6f s\n" rPar tPar
      printf "Abs diff:    %.3e\n" (abs (rSeq - rPar))
      printf "Speedup:     %.2fx\n\n" (tSeq / max 1e-12 tPar)
      exitSuccess
    else pure ()

  -- Otherwise perform ε-refinement using parallel Simpson
  let (res, nUsed) = refineUntilEps (simpsonPar chunk') f a' b' eps' n0' nMax'
  putStrLn "\n== ε-refinement (parallel) =="
  putStrLn $ "  result = " ++ show res
  putStrLn $ "  n used = " ++ show nUsed
  putStrLn   "\n Run with +RTS -s to see GC/CPU stats"
