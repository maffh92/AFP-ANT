{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Spec.Base
import           Spec.Optimization

import           Ant
import           Ant.Optimization

testOptimization :: Op -> AntMTest -> Bool
testOptimization opt antm =
  valid $ applyOpt (toOptimization opt)
                   (compileProg (toAntM antm))

main :: IO ()
main = hspec $
  prop "any possible optimization preserves validity" testOptimization


