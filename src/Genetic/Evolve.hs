{-# LANGUAGE TemplateHaskell #-}
module Genetic.Evolve
  ( newProgram
  , search
  , fitness
  ) where

import Data.Array.IO
import Ant.Base
import Ant.Monad
import Control.Lens
import Control.Monad
import Ant.Arbitrary.Base
import Test.QuickCheck
import Simulator
import Simulator.Base
import Genetic.ReadInstructions hiding (AntInstructions)

data GeneticConfig = GeneticConfig
  { _numRoundsPerGeneration :: Int -- Simulate this many rounds per program
  , _stopAfter              :: Int -- Stop search after this many generated programs
  , _linesPerFile           :: Int -- Amount of lines per file
  } deriving (Eq, Show)

makeLenses ''GeneticConfig

defaultGeneticConfig = GeneticConfig
  { _numRoundsPerGeneration = 10000
  , _stopAfter              = 1
  , _linesPerFile           = 10000
  }

-- | Generate a new program of length l
newProgram :: Int -> IO [Command L]
newProgram l = generate $ resize 1 $ newProgram' l
  where
    -- | Generate a random program of n lines
    newProgram' 0 = return []
    newProgram' n
      | n > 0 = f n
      | otherwise = fail "n is negative"
      where
        f k = do
          program <- generateProgram k
          let program' = unAntMTest program
          return program'

        generateProgram n = do
          let lbs = take n (iterate s z)
          AntMTest <$> replicateM n (genCommand lbs)

type Fitness = Int

search :: GeneticConfig -> IO [Command L]
search config =
  do -- Generate the initial pair of (program, fitness)
    prog      <- newProgram (view linesPerFile config)
    worldF    <- readFile "Genetic/sample0.world"
    blackProg <- readInstructions "Genetic/blackant.ant"

    _fitness <- fitness config worldF prog blackProg
    let e = (prog, _fitness)

    -- thunk of all the other programs to evaluate
    programs <- replicateM (view stopAfter config - 1) (newProgram (view linesPerFile config))


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

