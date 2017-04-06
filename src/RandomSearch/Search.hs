{-# LANGUAGE TemplateHaskell #-}
module RandomSearch.Search
  ( newProgram
  , search
  , defaultRandomSearchConfig
  , fitness
  ) where

import Ant.Arbitrary.Base
import Ant.Base
import Ant.Monad
import Control.Lens
import Control.Monad
import Simulator
import Simulator.Base
import Test.QuickCheck

-- | Configuration object with lenses
data RandomSearchConfig = RandomSearchConfig
  { _numRoundsPerGeneration :: Int -- Simulate this many rounds per program
  , _stopAfter              :: Int -- Stop search after this many generated programs
  , _linesPerFile           :: Int -- Amount of lines per file
  } deriving (Eq, Show)

makeLenses ''RandomSearchConfig

-- alias for documentation / readability
type Fitness = Int

defaultRandomSearchConfig :: RandomSearchConfig
defaultRandomSearchConfig = RandomSearchConfig
  { _numRoundsPerGeneration = 100000
  , _stopAfter              = 1000
  , _linesPerFile           = 10000
  }

-- | Generate a new program of length l
newProgram :: Label l => Int -> IO [Command l]
newProgram = generate . genProgram

-- | Brute force random search for a program with a high score. Search generates
-- a startpoint and its fitness, after which it will generate (N-1) other
-- programs and compare it to the current best program. If to programs are equal
-- in fitness, the eldest is kept.
search :: RandomSearchConfig -> IO [Command L]
search config =
  do prog1     <- newProgram (view linesPerFile config)
     worldF    <- readFile "../test-data/sample0.world"
     blackProg <- readInstructions "../test-data/blackant.ant"

     _fit1 <- fitness config worldF prog1 blackProg
     let e = (prog1, _fit1)

     programs <- replicateM (view stopAfter config - 1) . newProgram $ 10


     -- Evaluate all of them, keep the best one
     (best, _) <- foldM (\(_bp, _bf) p -> evalP config worldF _bf _bp blackProg p) e programs
     return best

-- | Will compare the result of a program to the score of the current best
-- program.
evalP
  :: RandomSearchConfig   -- ^ configuration of the randomised search
  -> String          -- ^ the world map, as a string
  -> Fitness         -- ^ Fitness of the best program so far
  -> [Command L]     -- ^ Current best program
  -> AntInstructions -- ^ Default program to benchmark against, for the black colony
  -> [Command L]     -- ^ The new program to test
  -> IO ([Command L], Fitness)
evalP config worldF _fit1 prog1 blackProg prog2 =
  do _fit2 <- fitness config worldF prog2 blackProg
     putStrLn ("Comparing a program with score " ++ show _fit1 ++ " to one that has fitness " ++ show _fit2)

     return $ case _fit1 `compare` _fit2 of
       LT -> (prog2, _fit2)
       _  -> (prog1, _fit1)

-- | Evaluate the program and extact the fitness.
fitness
  :: RandomSearchConfig -> String -> [Command L] -> AntInstructions -> IO Fitness
fitness config worldF prog blackProg  =
  do gstate  <- initGameState 0 worldF prog blackProg
     gstate' <- runNRounds (view numRoundsPerGeneration config) gstate
     return . redScore $ foodAdmin gstate'

