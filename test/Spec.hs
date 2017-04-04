{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Spec.Base
import           Spec.Optimization
import           Simulator

import           Ant
import           Ant.Optimization

main :: IO ()
main = hspec $ do
  propWith 1000 "any possible generated program is valid"
    testMonad

  propWith 1000 "any possible optimization preserves validity"
    testOptimization

  propWith 100 "any optimization preserves equivalence" $
    testOptimizationInSimulation 10000

-- | Test a optimization.
testOptimization :: Op -> AntMTest -> Bool
testOptimization opt antm =
  valid $ applyOpt (toOptimization opt)
                   (compileProg (toAntM antm))


-- | Test that the optimization preserves behaviour.
testOptimizationInSimulation :: Int -> Op -> Int -> AntMTest -> Property
testOptimizationInSimulation r opt seed antm =
  let cprog = compileProg (toAntM antm)
  in monadicIO $ do
    gs1  <- run $ initGameState seed tinyWorld cprog >>= runNRounds r
    gs2  <- run $ initGameState seed tinyWorld (applyOpt (toOptimization opt)
                                               cprog) >>= runNRounds r
    run (gs1 =~= gs2)

-- | Test validity of programs
testMonad :: AntMTest -> Bool
testMonad = valid . compileProg . toAntM


--------------------------------------------------------------------------------
  -- Utils

-- | A `heavier` version of prop where the number of runs
-- can be specified.
propWith :: Testable prop => Int -> String -> prop -> Spec
propWith n str = modifyMaxSuccess (const n) . prop str

-- | Example world
tinyWorld = unlines
  ["10"
  ,"10"
  ,"# # # # # # # # # #"
  ,"# 9 9 . . . . 3 3 #"
  ,"# 9 # . . . . . . #"
  ,"# . # . . . . . . #"
  ,"# . . 5 . . . . . #"
  ,"# . . . . . 5 . . #"
  ,"# . . . . . . # . #"
  ,"# . . . + . . # 9 #"
  ,"# 3 3 . . . . 9 9 #"
  ," # # # # # # # # # #"]
