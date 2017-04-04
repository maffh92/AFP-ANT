{-# LANGUAGE TemplateHaskell #-}
module Genetic.Evolve
  ( newProgram
  , search
  , defaultGeneticConfig
  , fitness
  ) where

import Ant.Arbitrary.Base
import Ant.Base
import Ant.Monad
import Control.Lens
import Control.Monad
import Data.Array.IO
import Simulator
import Simulator.Base
import Strategy
import Test.QuickCheck

data GeneticConfig = GeneticConfig
  { _numRoundsPerGeneration :: Int -- Simulate this many rounds per program
  , _stopAfter              :: Int -- Stop search after this many generated programs
  , _linesPerFile           :: Int -- Amount of lines per file
  } deriving (Eq, Show)

makeLenses ''GeneticConfig

defaultGeneticConfig = GeneticConfig
  { _numRoundsPerGeneration = 10000
  , _stopAfter              = 10
  , _linesPerFile           = 10000
  }

-- | Generate a new program of length l
newProgram :: Label l => Int -> IO [Command l]
newProgram = generate . genProgram

type Fitness = Int

search :: GeneticConfig -> IO [Command L]
search config =
  do -- Generate the initial pair of (program, fitness)
    prog      <- newProgram (view linesPerFile config)
    worldF    <- readFile "../test-data/sample0.world"
    blackProg <- readInstructions "../test-data/blackant.ant"

    _fitness <- fitness config worldF prog blackProg
    let e = (prog, _fitness)

    -- thunk of all the other programs to evaluate
    programs <- replicateM (view stopAfter config - 1)
                           (newProgram 10)


    -- Evaluate all of them, keep the best one
    (best, _) <- foldM (\(_bp, _bf) p -> evalP config worldF _bf _bp blackProg p) e programs
    return best

-- given a program and it's score, and another program, evaluate the score of
-- the second program and compare it to the score of the first. The best one is
-- kept and the other is discarded.
evalP
  :: GeneticConfig   -- ^ configuration of the randomised search
  -> String          -- ^ the world map, as a string
  -> Fitness         -- ^ Fitness of the best program so far
  -> [Command L]     -- ^ Current best program
  -> AntInstructions -- ^ Default program to benchmark against, for the black colony
  -> [Command L]     -- ^ The new program to test
  -> IO ([Command L], Fitness)
evalP config worldF _fitness1 prog1 blackProg prog2 =
  do
    _fitness2 <- fitness config worldF prog2 blackProg
    putStrLn ("Comparing a program with score " ++ show _fitness1 ++ " to one that has fitness " ++ show _fitness2)

    return $ case _fitness1 `compare` _fitness2 of
      LT -> (prog2, _fitness2)
      EQ -> (prog1, _fitness1)
      GT -> (prog1, _fitness1)

-- | Evaluate the program, use the final score as fitness
fitness
  :: GeneticConfig
  -> String
  -> [Command L]
  -> AntInstructions
  -> IO Fitness
fitness config worldF prog blackProg  =
  do
    gstate  <- initGameState 0 worldF prog blackProg
    gstate' <- runNRounds (view numRoundsPerGeneration config) gstate
    return . redScore $ foodAdmin gstate'

