module NumericsSpec (spec) where

import Test.Hspec
import Numerics.Integrate

spec :: Spec
spec = do
  describe "Simpson integration" $ do

    it "computes ∫₀^π sin(x) dx ≈ 2.0" $ do
      let (res, _) = refineUntilEps 512 sin 0 pi 1e-9 1024 (8*1024*1024)
      res `shouldSatisfy` (\x -> abs (x - 2.0) < 1e-6)

    it "computes ∫₀¹ (x+1)^2 dx = 7/3 ≈ 2.3333" $ do
      let f = \x -> x*x + 2*x + 1
          (res, _) = refineUntilEps 256 f 0 1 1e-9 512 1048576
      res `shouldSatisfy` (\x -> abs (x - (7/3)) < 1e-6)

    it "converges faster with higher n0" $ do
      let f = sin
          (r1, n1) = refineUntilEps 128 f 0 pi 1e-6 128 1048576
          (r2, n2) = refineUntilEps 128 f 0 pi 1e-6 4096 1048576
      abs (r1 - r2) `shouldSatisfy` (< 1e-4)
      n2 `shouldSatisfy` (>= 4096)
