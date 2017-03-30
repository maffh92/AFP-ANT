module Genetic.Evolve
  ( fresh
  , mutate
  , fitness
  ) where

import Ant.Base
import Ant.Monad
import Control.Monad
import Ant.Arbitrary.Base
import Test.QuickCheck

generateProgram :: Int -> Gen AntMTest
generateProgram n = do
  let lbs = take n (iterate s z)
  AntMTest <$> replicateM n (genCommand lbs)

-- | Generate a random program of n lines
fresh :: Int -> Gen [Command L]
fresh n = do
  program <- generateProgram 10
  let program' = unAntMTest program
  return program'

mutate :: Gen [Command L] -> Gen [Command L]
mutate = undefined

-- | Evaluate the program, use the final score as fitness
fitness :: Gen [Command L] -> Gen Int
fitness = undefined
