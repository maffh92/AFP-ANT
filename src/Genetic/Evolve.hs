{-# LANGUAGE TemplateHaskell #-}
module Genetic.Evolve
  ( newProgram
  , search
  , fitness
  ) where

import Ant.Base
import Ant.Monad
import Control.Lens
import Control.Monad
import Ant.Arbitrary.Base
import Test.QuickCheck
import Simulator
import Simulator.Base

data GeneticConfig = GeneticConfig
  { _numRoundPerGeneration :: Int -- Simulate this many rounds per program
  , _stopAfter             :: Int -- Stop search after this many generated programs
  , _linesPerFile          :: Int -- Amount of lines per file
  } deriving (Eq, Show)

makeLenses ''GeneticConfig

defaultGeneticConfig = GeneticConfig
  { _numRoundPerGeneration = 10000
  , _stopAfter             = 10000
  , _linesPerFile          = 10000
  }

generateProgram :: Int -> Gen AntMTest
generateProgram n = do
  let lbs = take n (iterate s z)
  AntMTest <$> replicateM n (genCommand lbs)

newProgram :: GeneticConfig -> IO [Command L]
newProgram config = generate $ resize 1 $ newProgram' (view linesPerFile config)

-- | Generate a random program of n lines
newProgram' :: Int -> Gen [Command L]
newProgram' 0 = return []
newProgram' n
  | n > 0 = f n
  | otherwise = fail "n is negative"
  where
    f k = do
      program <- generateProgram k
      let program' = unAntMTest program
      return program'

type Fitness = Int
type ProgramPair = ([Command L], Fitness)

search :: GeneticConfig -> IO [Command L]
search config =
  do -- Generate the initial pair of (program, fitness)
    prog     <- newProgram config

    worldF <- readFile "...somefile"

    _fitness <- fitness config worldF prog
    let e = (prog, _fitness)

    -- thunk of all the other programs to evaluate
    programs <- replicateM (view stopAfter config - 1) (newProgram config)

    -- Evaluate all of them, keep the best one
    (best, _) <- foldM (evalP config worldF) e programs
    return best

-- given a program and it's score, and another program, evaluate the score of
-- the second program and compare it to the score of the first. The best one is
-- kept and the other is discarded.
evalP :: GeneticConfig -> String -> ProgramPair -> [Command L] -> IO ProgramPair
evalP config worldF (prog1, _fitness1) prog2 =
  do
    _fitness2 <- fitness config worldF prog2

    return $ case _fitness1 `compare` _fitness2 of
      LT -> (prog2, _fitness2)
      EQ -> (prog1, _fitness1)
      GT -> (prog1, _fitness1)


-- | Evaluate the program, use the final score as fitness
fitness :: GeneticConfig -> String -> [Command L] -> IO Fitness
fitness config worldF prog =
  do
    gstate  <- initGameState 0 worldF prog
    gstate' <- runNRounds (view numRoundPerGeneration config) gstate
    return (redScore $ foodAdmin gstate')
