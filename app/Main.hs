module Main where

import System.Environment (getArgs)
import GHC.Conc          (setNumCapabilities)
import Numerics.Integrate

data Args = Args
  { a       :: Double
  , b       :: Double
  , eps     :: Double
  , threads :: Int
  , func    :: String
  , n0      :: Int
  , nMax    :: Int
  } deriving Show

defaults :: Args
defaults = Args { a = 0, b = 1, eps = 1e-8, threads = 0, func = "sin", n0 = 1024, nMax = 8*1024*1024 }

parseArgs :: [String] -> Args
parseArgs = go defaults
  where
    go acc [] = acc
    go acc ("--a":x:xs)       = go acc{a=read x} xs
    go acc ("--b":x:xs)       = go acc{b=read x} xs
    go acc ("--eps":x:xs)     = go acc{eps=read x} xs
    go acc ("--threads":x:xs) = go acc{threads=read x} xs
    go acc ("--func":x:xs)    = go acc{func=x} xs
    go acc ("--n0":x:xs)      = go acc{n0=read x} xs
    go acc ("--nmax":x:xs)    = go acc{nMax=read x} xs
    go _   (bad:_)            = error $ "Unknown arg: " ++ bad

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  if threads args > 0
     then setNumCapabilities (threads args)
     else pure ()

  let f         = chooseFunc (func args)
      a'        = a args
      b'        = b args
      eps'      = eps args
      n0'       = n0 args
      nMax'     = nMax args
      chunk     = max 512 (case threads args of 0 -> 1024; t -> 32*t)

  putStrLn $ "Integrating (parallel Simpson):"
  putStrLn $ "  f     = " ++ func args
  putStrLn $ "  [a,b] = [" ++ show a' ++ ", " ++ show b' ++ "]"
  putStrLn $ "  eps   = " ++ show eps'
  putStrLn $ "  n0    = " ++ show n0' ++ ", nMax = " ++ show nMax'
  putStrLn $ "  chunk = " ++ show chunk

  let (res, nUsed) = refineUntilEps chunk f a' b' eps' n0' nMax'
  putStrLn $ "\nResult: " ++ show res
  putStrLn $ "n used: " ++ show nUsed
  putStrLn   "Tip: run with +RTS -s to see GC/CPU stats"
