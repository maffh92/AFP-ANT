{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Spec.Base
import           Spec.Optimization

import           Ant
import           Ant.Optimization

testOptimization :: Op -> AntMTest -> Bool
testOptimization opt antm =
  valid $ applyOpt (toOptimization opt)
                   (compileProg (toAntM antm))


testMonad :: AntMTest -> Bool
testMonad = valid . compileProg . toAntM

main :: IO ()
main = hspec $ do
  prop "any possible generated program is well formed"
    testMonad

  prop "any possible optimization preserves validity"
    testOptimization


