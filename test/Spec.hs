{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Ant.Arbitrary
import           Simulator

import           Ant
import           Ant.Optimization

import           Spec.Optimization

-- | Entry point for test
main :: IO ()
main = hspec $ do
  propWith 1000 "any possible generated program is valid"
    testValidity

  propWith 1000 "any possible optimization preserves validity"
    testOptimization

  propWith 100 "any optimization preserves equivalence" $
    testOptimizationInSimulation 100000

-- | Test a optimization.
testOptimization :: Op -> AntMTest L -> Bool
testOptimization opt antm =
  testValidity antm &&
  valid (applyOpt (toOptimization opt)
                  (compileProg (toAntM antm)))

-- | Test that the optimization preserves behaviour.
testOptimizationInSimulation :: Int -> Op -> Int -> AntMTest L -> Property
testOptimizationInSimulation r opt seed antmtest =
  let cprog = compileProg (toAntM antmtest)
  in monadicIO $ do
    -- load some file to be as blackInstr
    blackInstr <- run $ readInstructions "test-data/dunkosmiloolump-1.ant"

    -- simple alias for running a given program
    let initP p = run $ initGameState seed tinyWorld p blackInstr

    -- initialize state without the optimization
    gs1  <- initP (toCmds cprog)
    -- initialize state with the optimization
    gs2  <- initP (toCmds $ applyOpt (toOptimization opt) cprog)

    -- run both programs r times
    gs1' <- run $ runNRounds r gs1
    gs2' <- run $ runNRounds r gs2

    -- compare the final states, if we compare all states
    -- is just too slow
    run $ gs1' =~= gs2'

-- | Test validity of programs
testValidity :: AntMTest L -> Bool
testValidity = valid . compileProg . toAntM


--------------------------------------------------------------------------------
  -- Utils

-- | A `heavier` version of prop where the number of runs
-- can be specified.
propWith :: Testable prop => Int -> String -> prop -> Spec
propWith n str = modifyMaxSuccess (const n) . prop str

-- | Example world for testing purposes
tinyWorld :: String
tinyWorld = unlines
  ["10"
  ,"10"
  ,"# # # # # # # # # #"
  ,"# 9 9 . . . . 3 3 #"
  ,"# 9 # . - - . . . #"
  ,"# . # . . . . . . #"
  ,"# . . 5 . . . . . #"
  ,"# . . . . . 5 . . #"
  ,"# . . . . . . # . #"
  ,"# . . . + + . # 9 #"
  ,"# 3 3 . . . . 9 9 #"
  ," # # # # # # # # # #"]
