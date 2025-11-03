module SimpsonSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Numerics.Funcs
import Numerics.SimpsonSeq
import Numerics.SimpsonPar

spec :: Spec
spec = do

  describe "Simpson Rule — Mathematical Correctness" $ do

    it "∫₀^π sin(x) dx = 2 (sequential)" $ do
      let f = chooseFunc "sin"
      let approx = simpsonSeq f 0 pi 4096
      abs (approx - 2.0) `shouldSatisfy` (< 1e-8)

    it "∫₀^1 (x+1)^2 dx = 7/3 (sequential)" $ do
      let f = chooseFunc "poly"
      let approx = simpsonSeq f 0 1 4096
      abs (approx - (7/3)) `shouldSatisfy` (< 1e-8)

  describe "Sequential vs Parallel Consistency" $ do

    it "Parallel gives the same result as sequential for sin on [0, π]" $ do
      let f = chooseFunc "sin"
      let n = 131072
      let seqVal = simpsonSeq f 0 pi n
      let parVal = simpsonPar 1024 f 0 pi n
      abs (seqVal - parVal) `shouldSatisfy` (< 1e-10)

    it "Parallel matches sequential for exp on [0, 1]" $ do
      let f = chooseFunc "exp"
      let n = 65536
      let seqVal = simpsonSeq f 0 1 n
      let parVal = simpsonPar 512 f 0 1 n
      abs (seqVal - parVal) `shouldSatisfy` (< 1e-10)

  describe "Input Validation" $ do

    it "Rejects odd subdivision counts (n must be even)" $ do
      let f = chooseFunc "sin"
      evaluate (simpsonSeq f 0 1 5) `shouldThrow` anyErrorCall
      evaluate (simpsonPar 64 f 0 1 5) `shouldThrow` anyErrorCall
