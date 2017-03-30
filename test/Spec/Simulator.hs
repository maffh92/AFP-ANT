{-# LANGUAGE FlexibleContexts #-}
module Spec.Simulator where

import Simulator
import Ant.Base
import Ant.Monad
import           Data.Array.IO
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (replicateM_)
import Data.Char (isDigit)

import Control.Monad.State

--
toAntInstructions :: Label l => Program l -> IO AntInstructions
toAntInstructions prog =
  let cmds = prog ^. commands
   in newListArray (0 , M.size cmds - 1)
                   (map (fmap toInt . snd) . M.toAscList $ cmds)

-- type World = IOArray Pos Cell
(=~=) :: GameState -> GameState -> IO Bool
gs1 =~= gs2 = liftM2 (==) (getElems (world gs1)) (getElems (world gs2))


-- data GameState = GameState
--    { world             :: World
--    , redInstructions   :: AntInstructions
--    , blackInstructions :: AntInstructions
--    , antPositions      :: AntPositions
--    , randoms           :: [Int]
--    , roundNumber       :: Int
--    , foodAdmin         :: FoodAdmin
--    }

runNRounds :: Int -> GameState -> IO GameState
runNRounds n = runSimulator (replicateM_ n oneRound)

-- | Just one red Ant in the middle of a board
initGameState :: Label l
              => Int         -- ^ Random seed
              -> Program l   -- ^ Program
              -> IO GameState
initGameState seed prog =
   do theWorld   <- mkWorld
      redInstr   <- toAntInstructions prog
      blackInstr <- newListArray (0,0) []

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


oneRound :: Sim ()
oneRound =
  do modify (\game -> game {roundNumber = roundNumber game + 1})
     pm   <- gets antPositions
     list <- liftIO $ getAssocs pm
     mapM_ step list

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

mkWorld :: IO World
mkWorld =
	let s = tinyWorld
  in case map words (lines s) of
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
