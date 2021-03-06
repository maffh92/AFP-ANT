{-# LANGUAGE FlexibleContexts #-}
module Simulator
  ( toAntInstructions
  , runNRounds
  , initGameState
  , GameState(..)
  , (=~=)
  , readInstructions
  ) where

import           Control.Monad.State
import           Data.Array.IO
import           Data.Char           (isDigit, toUpper)
import qualified Data.Set            as S

import           Simulator.Base
import           Ant.Monad
import           Ant.Base

-- | Generate AntInstructions from a Program.
-- This leaves in IO because AntInstructions is a IOArray.
toAntInstructions :: Label l
                  => [Command l]
                  -> IO AntInstructions
toAntInstructions cmds =
   newListArray (0 , length cmds - 1)
                (map (fmap toInt) cmds)

-- | Compare two GameState for equivalencea.
-- Because under the hood uses IOArray this has
-- to leave in IO.
(=~=) :: GameState -> GameState -> IO Bool
gs1 =~= gs2 =
  -- Equality on the results
  ((foodAdmin gs1 == foodAdmin gs2) &&) <$>
  -- Equality on the world final state
  liftM2 (==) (getElems (world gs1)) (getElems (world gs2))

runRound :: GameState -> IO GameState
runRound = runSimulator oneRound

runNRounds :: Int -> GameState -> IO GameState
runNRounds n =
  execStateT (replicateM n $ get >>= liftIO . runRound >>= put)

-- | Initialize game state.
initGameState :: Label l
              => Int              -- ^ Random seed
              -> String           -- ^ String representing the world
              -> [Command l]  -- ^ red ant program
              -> AntInstructions  -- ^ black ant program
              -> IO GameState
initGameState seed w rprog blackInstr =
   do theWorld   <- mkWorld w
      redInstr   <- toAntInstructions rprog
      (pm, foodPos, foodParticles) <- populateWorld theWorld

      return $ GameState
         { world             = theWorld
         , redInstructions   = redInstr
         , blackInstructions = blackInstr
         , antPositions      = pm
         , randoms           = randomStream seed
         , roundNumber       = 0
         , foodAdmin         = noFood { remaining = foodParticles, locations = foodPos }
         }

-- | Run the simulation for one round.
oneRound :: Sim ()
oneRound =
  do modify (\game -> game {roundNumber = roundNumber game + 1})
     pm   <- gets antPositions
     list <- liftIO $ getAssocs pm
     mapM_ step list

-- | Populate a world placing Ants as defined in the specification
populateWorld :: World -> IO (AntPositions, S.Set Pos, Int)
populateWorld theWorld =
   do list <- getAssocs theWorld
      (nrOfAnts, pm, foodSet, nrOfFood) <- foldM op (0, [], S.empty, 0) list
      arr <- newListArray (0, nrOfAnts - 1) (reverse pm)
      return (arr, foodSet, nrOfFood)
 where
   op this@(i, pm, fm, f) (pos, cell)
      | food cell > 0 = return (i, pm, S.insert pos fm, f + food cell)
      | otherwise =
           case anthill cell of
              Nothing ->
                 return this
              Just c ->
                 do writeArray theWorld pos (cell { antInCell = Just (makeAnt i c) })
                    return (i+1, Just pos : pm, fm, f)

-- | Make a single Ant
makeAnt :: Int -> AntColor -> Ant
makeAnt i c = Ant
   { antId        = i
   , antColor     = c
   , antState     = 0
   , antResting   = 0
   , antDirection = 0
   , antHasFood   = False
   }

--------------------------------------------------------------------------------
  -- World

-- | Make a world out of a String.
-- The specific format of the String to be a valid
-- world is explained in the specification.
mkWorld :: String -> IO World
mkWorld s =
  case map words (lines s) of
    [dimX] : [dimY] : rest | isNum dimX && isNum dimY ->
      do let cells = map stringToCell (concat rest)
             dx    = read dimX
             dy    = read dimY
         w <- newListArray (Pos 0 0, Pos (dx-1) (dy-1)) cells
         if length cells /= dx * dy
            then error "invalid world"
            else return w
    _ -> error "invalid world"

stringToCell :: String -> Cell
stringToCell s =
   case s of
      ['#'] -> stdCell { cellType = Rocky }
      ['.'] -> stdCell
      ['+'] -> stdCell { anthill = Just Red }
      ['-'] -> stdCell { anthill = Just Black }
      [_] | isNum s -> stdCell { food = read s }
      _ -> error ("invalid cell in world file: " ++ show s)

stdCell :: Cell
stdCell = Cell { antInCell    = Nothing
               , cellType     = Clear
               , food         = 0
               , anthill      = Nothing
               , markersRed   = noMarkers
               , markersBlack = noMarkers
               }

isNum :: String -> Bool
isNum s = all isDigit s && not (null s)

--------------------------------------------------------------------------------
  -- Instructions

readInstructions :: String -> IO AntInstructions
readInstructions antFile =
   do input <- readFile antFile
      let list = map (fst . stringToInstruction) (lines (map toUpper input))
          len  = length list
      arr <- newListArray (0, len-1) list
      if len > 10000
        then error ("Too many states: " ++ show antFile)
        else return arr

stringToInstruction :: String -> (Instruction, Maybe String)
stringToInstruction string =
   let (instr, rest) = break (==';') string
       comment = if null rest then Nothing else Just (tail rest)
       instruction =
          case words instr of
             ["SENSE", sensedir, st1, st2, cond] ->
                Sense (readSenseDir sensedir) (readST st1) (readST st2) (readCond cond)
             ["SENSE", sensedir, st1, st2, "MARKER", i] ->
                Sense (readSenseDir sensedir) (readST st1) (readST st2) (Marker (readI i))
             ["MARK", i, st] ->
                Mark (readI i) (readST st)
             ["UNMARK", i, st] ->
                Unmark (readI i) (readST st)
             ["PICKUP", st1, st2] ->
                PickUp (readST st1) (readST st2)
             ["DROP", st] ->
                Drop (readST st)
             ["TURN", lr, st] ->
                Turn (readLR lr) (readST st)
             ["MOVE", st1, st2] ->
                Move (readST st1) (readST st2)
             ["FLIP", p, st1, st2] ->
                Flip (readP p) (readST st1) (readST st2)
             _ -> error ("Unknown instruction: " ++show instr)
   in (instruction, comment)

  where
    readSenseDir s =
       case s of
          "HERE"       -> Here
          "AHEAD"      -> Ahead
          "LEFTAHEAD"  -> LeftAhead
          "RIGHTAHEAD" -> RightAhead
          _            -> error "invalid sensedir"

    readCond s =
       case s of
          "FRIEND"         -> Friend
          "FOE"            -> Foe
          "FRIENDWITHFOOD" -> FriendWithFood
          "FOEWITHFOOD"    -> FoeWithFood
          "FOOD"           -> Food
          "ROCK"           -> Rock
          "FOEMARKER"      -> FoeMarker
          "HOME"           -> Home
          "FOEHOME"        -> FoeHome
          _                -> error "invalid condition"

    readLR s =
       case s of
          "LEFT"  -> IsLeft
          "RIGHT" -> IsRight
          _       -> error "invalid LeftRight value"

    readST  = (\i -> if i > 9999 then error ("State >9999: " ++ show i) else i) . readP
    readI   = toEnum . readP
    readP s = if isNum s then read s else error ("Not a number: " ++ s)
