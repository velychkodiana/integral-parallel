module RefinementSpec (spec) where

import Test.Hspec
import Text.Printf (printf)
import Control.Monad (when)

import Numerics.Funcs
import Numerics.Refine
import Numerics.SimpsonPar

spec :: Spec
spec = describe "Adaptive refinement of subintervals to reach target ε" $ do

  it "Refinement achieves requested precision on ∫₀^π sin(x) dx" $ do
    let f = chooseFunc "sin"
        eps = 1e-8
        (result, _) = refineUntilEps (simpsonPar 1024) f 0 pi eps 1024 (8*1024*1024)
    abs (result - 2.0) `shouldSatisfy` (< eps)

  it "Looser ε should not require more subintervals than stricter ε" $ do
    let f = chooseFunc "exp"
        ( _, nStrict) = refineUntilEps (simpsonPar 512) f 0 1 1e-10 1024 (8*1024*1024)
        ( _, nLoose ) = refineUntilEps (simpsonPar 512) f 0 1 1e-6  1024 (8*1024*1024)
    when (nLoose > nStrict) $
      expectationFailure (printf "Expected nLoose <= nStrict, but got nLoose=%d nStrict=%d" nLoose nStrict)
    nLoose `shouldSatisfy` (<= nStrict)
