{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property

import           Spec.Base
import           Spec.Optimization

import           Ant
import           Ant.Optimization


main :: IO ()
main = hspec $ do
  propWith 1000 "any possible generated program is well formed"
    testMonad

  propWith 1000 "any possible optimization preserves validity"
    testOptimization

-- | Test a optimization
testOptimization :: Op -> AntMTest -> Bool
testOptimization opt antm =
  valid $ applyOpt (toOptimization opt)
                   (compileProg (toAntM antm))


-- | Test validity of programs
testMonad :: AntMTest -> Bool
testMonad = valid . compileProg . toAntM


--------------------------------------------------------------------------------
  -- Utils

-- | A `heavier` version of prop where the number of runs
-- can be specified.
propWith :: Testable prop => Int -> String -> prop -> Spec
propWith n str = modifyMaxSuccess (const n) . prop str
