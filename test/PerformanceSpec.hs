module PerformanceSpec (spec) where

import Test.Hspec
import Numerics.Funcs
import Numerics.SimpsonSeq
import Numerics.SimpsonPar

spec :: Spec
spec = describe "Performance sanity checks" $ do
  it "parallel matches sequential at moderate n" $ do
    let f = chooseFunc "sin"
        n = 20000
        rSeq = simpsonSeq f 0 pi n
        rPar = simpsonPar 500 f 0 pi n
    abs (rSeq - rPar) `shouldSatisfy` (< 1e-6)
