module Numerics.Funcs
  ( chooseFunc
  , availableFuncs
  ) where

-- | Map friendly names to functions for integration.
--   Feel free to add more if needed.
chooseFunc :: String -> (Double -> Double)
chooseFunc name =
  case name of
    "sin"  -> sin
    "cos"  -> cos
    "exp"  -> exp
    "poly" -> \x -> (x + 1) * (x + 1)       -- (x+1)^2
    "cube" -> \x -> x*x*x                   -- x^3
    _      -> error "Unknown --func (use: sin|cos|exp|poly|cube)"

availableFuncs :: [String]
availableFuncs = ["sin","cos","exp","poly","cube"]
