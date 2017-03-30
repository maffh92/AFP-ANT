{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Spec.Base
import           Spec.Optimization
import Spec.Simulator

import           Ant
import           Ant.Optimization

main :: IO ()
main = hspec $ do
  propWith 1000 "any possible generated program is valid"
    testMonad

  propWith 1000 "any possible optimization preserves validity"
    testOptimization

  propWith 10 "any optimization preserves equivalence" $
    testOptimizationInSimulation 10

-- | Test a optimization
testOptimization :: Op -> AntMTest -> Bool
testOptimization opt antm =
  valid $ applyOpt (toOptimization opt)
                   (compileProg (toAntM antm))


testOptimizationInSimulation :: Int -> Int -> Op -> AntMTest -> Property
testOptimizationInSimulation r seed opt antm =
  let cprog = compileProg (toAntM antm)
  in monadicIO $ do
    gs1  <- run $ initGameState seed cprog
    gs2  <- run $ initGameState seed $ applyOpt (toOptimization opt) cprog
    gs1' <- run $ runNRounds r gs1
    gs2' <- run $ runNRounds r gs2
    run (gs1' =~= gs2')

-- | Test validity of programs
testMonad :: AntMTest -> Bool
testMonad = valid . compileProg . toAntM


--------------------------------------------------------------------------------
  -- Utils

-- | A `heavier` version of prop where the number of runs
-- can be specified.
propWith :: Testable prop => Int -> String -> prop -> Spec
propWith n str = modifyMaxSuccess (const n) . prop str
