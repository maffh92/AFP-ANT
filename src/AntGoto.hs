module AntGoto where

import qualified AntState as A
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Fix

{- describes an AST for a language with named labels and gotos -}

{- commands -} 
data Command = -- labels
               Label String
             | Goto String
               -- non-branching commands
             | Mark A.Marker
             | Unmark A.Marker
             | Drop
             | Turn A.TurnDir
               -- branching commants 
             | PickUp String String
             | Sense A.SenseDir String String A.Condition
             | Move String String
             | Flip Int String String
             deriving (Eq,Show)

isLabel :: Command -> Bool
isLabel (Label _) = True
isLabel _         = False

{- intermediate stage w/explicit marking of states -}
data S2Lbl = S2State Int | S2Lbl String -- label or known state #
             deriving (Eq,Show)

data S2Cmd = S2Sense A.SenseDir S2Lbl S2Lbl A.Condition
           | S2Mark A.Marker S2Lbl
           | S2Unmark A.Marker S2Lbl
           | S2PickUp S2Lbl S2Lbl
           | S2Drop S2Lbl
           | S2Turn A.TurnDir S2Lbl
           | S2Move S2Lbl S2Lbl
           | S2Flip Int S2Lbl S2Lbl
           deriving (Eq,Show)

{- example -}
example :: [Command]
example = [ Label "SenseFood"
          ,   Sense A.Ahead "Food" "NoFood" A.Food
          , Label "Food"
          ,   Move "Eat" "SenseFood"
          , Label "Eat"
          ,   PickUp "SenseHome" "SenseFood"
          , Label "NoFood"
          ,   Flip 3 "GoLeft" "RightOrForward"
          , Label "GoLeft" 
          ,   Turn A.Left
          ,   Goto "SenseFood"
          , Label "RightOrForward"
          ,   Flip 2 "GoRight" "GoForward"
          , Label "GoRight"
          ,   Turn A.Right
          ,   Goto "SenseFood"
          , Label "GoForward"
          ,   Move "SenseFood" "NoFood"
           
          , Label "SenseHome"
          ,   Sense A.Ahead "Home" "NotHome" A.Home
          , Label "Home"
          ,   Move "DropFood" "SenseHome"
          , Label "DropFood"
          ,   Drop
          ,   Goto "SenseFood"
          , Label "NotHome"
          ,   Flip 3 "HGoLeft" "HRightOrForward"
          , Label "HGoLeft"
          ,   Turn A.Left
          ,   Goto "SenseHome"
          , Label "HRightOrForward"
          ,   Flip 2 "HGoRight" "HGoForward"
          , Label "HGoRight"
          ,   Turn A.Right
          ,   Goto "SenseHome"
          , Label "HGoForward"
          ,   Move "SenseHome" "NotHome"
          ]

{- compile the goto language into the ant state description -}
compile :: [Command] -> [A.Command Int]
compile cmds = compile_stage2 $ resolve lbls cmds' 
    where (cmds', lbls) = compile_stage1 0 $ relabel cmds

type LabelMap = M.Map String String

{- find a fixpoint -}
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint fn val | val == val' = val
                | otherwise   = fixpoint fn val'
    where val' = fn val  

relabel :: [Command] -> [Command]
relabel cmds = fixpoint (step findAndRemove) $ cmds


{- rename doubled-up labels (e.g. [Label x, Label y, Label z] -}
renameDoubled :: LabelMap -> [Command] -> (LabelMap, [Command])
renameDoubled mapping []   = (mapping, [])
renameDoubled mapping cmds | null labels = let (mapping'', rest') = renameDoubled mapping $ tail cmds
                                           in  (mapping'', (head cmds):rest')
                           | otherwise   = let (mapping'', rest') = renameDoubled mapping' rest
                                           in  (mapping'', (Label first):rest)
    where labels    = takeWhile isLabel cmds
          rest      = dropWhile isLabel cmds 
                    
          (Label first) = head labels
          others        = tail labels
                  
          mapfn = foldl (.) id $ map (\(Label lbl) -> M.insert lbl first) others
          mapping' = mapfn mapping


{- find and remove label/goto pairs and doubled labels -}
findAndRemove :: LabelMap -> [Command] -> (LabelMap, [Command])
findAndRemove map []                        = (map, [])
findAndRemove map ((Label x):(Goto y):rest) = let map' = M.insert x y map
                                              in  findAndRemove map' rest
findAndRemove map (cmd:cmds)                = let (map', cmds') = findAndRemove map cmds
                                              in  (map', cmd:cmds')
          
{- relabel a command using a mapping -}

findNewLabel :: LabelMap -> String -> String
findNewLabel map lbl = fromMaybe lbl $ M.lookup lbl map

relmapCmd :: LabelMap -> Command -> Command
relmapCmd map (Goto x)        = Goto $ findNewLabel map x
relmapCmd map (PickUp x y)    = PickUp (findNewLabel map x) (findNewLabel map y)
relmapCmd map (Sense d x y c) = Sense d (findNewLabel map x) (findNewLabel map y) c
relmapCmd map (Move x y)      = Move (findNewLabel map x) (findNewLabel map y)
relmapCmd map (Flip p x y)    = Flip p (findNewLabel map x) (findNewLabel map y)
relmapCmd _   cmd             = cmd 

{- do one replacement step -}
step :: (LabelMap -> [Command] -> (LabelMap, [Command])) -> [Command] -> [Command]
step fn cmds = let (map', cmds') = fn M.empty cmds 
               in  map (relmapCmd map') cmds'
          
          

{- first stage compilation: figure out state sequence and label positions -}
compile_stage1 :: Int -> [Command] -> ([S2Cmd], [(String, Int)])
compile_stage1 _        []         = ([], [])
compile_stage1 curstate (cmd:cmds) = (s2cmds++nextcmds, lbl++nextlabels)
    where -- state to jump to
          jmpstate = case cmds of 
                         []           -> S2State 0                -- last cmd; program "wraps around"
                         ((Goto l):_) -> S2Lbl l                  -- goto; go there next
                         _            -> S2State $ succ curstate  -- sequence; go to next cmd
          
          -- any labels added?
          lbl      = case cmd of
                         (Label lbl) -> [(lbl, curstate)] -- keep track of where the label is
                         _           -> []                -- no label

          -- command
          s2cmds   = case cmd of 
                         (Label _)   -> []    -- labels have no corresponding command
                         (Goto _)    -> []    -- neither do gotos
                         -- non-branching commands
                         (Mark m)    -> [S2Mark m jmpstate]
                         (Unmark m)  -> [S2Unmark m jmpstate]
                         (Drop)      -> [S2Drop jmpstate]
                         (Turn d)    -> [S2Turn d jmpstate]
                         -- branching commands
                         (Sense sd s1 s2 c) -> [S2Sense sd (S2Lbl s1) (S2Lbl s2) c]
                         (PickUp s1 s2)     -> [S2PickUp (S2Lbl s1) (S2Lbl s2)]
                         (Move s1 s2)       -> [S2Move (S2Lbl s1) (S2Lbl s2)]
                         (Flip p s1 s2)     -> [S2Flip p (S2Lbl s1) (S2Lbl s2)]

          -- next state
          nxstate = curstate +  length s2cmds
  
          -- rest of the program
          (nextcmds, nextlabels) = compile_stage1 nxstate cmds

{- resolve labels to cmds in stage 2 -}
resolve :: [(String, Int)] -> [S2Cmd] -> [S2Cmd]
resolve _    []         = []
resolve lbls (cmd:cmds) = cmd' : resolve lbls cmds
    where rslv :: S2Lbl -> S2Lbl
          rslv (S2State x) = S2State x -- don't change fixed param
          rslv (S2Lbl lbl) = S2State x
              where x = case lookup lbl lbls of
                            (Just x') -> x'
                            Nothing   -> error ("Undefined label: " ++ lbl)

          cmd' = case cmd of
                      (S2Sense sd s1 s2 cond) -> S2Sense sd (rslv s1) (rslv s2) cond
                      (S2Mark m st)           -> S2Mark m (rslv st)
                      (S2Unmark m st)         -> S2Unmark m (rslv st)
                      (S2PickUp s1 s2)        -> S2PickUp (rslv s1) (rslv s2)
                      (S2Drop st)             -> S2Drop (rslv st)
                      (S2Turn dir st)         -> S2Turn dir (rslv st)
                      (S2Move s1 s2)          -> S2Move (rslv s1) (rslv s2)
                      (S2Flip p s1 s2)        -> S2Flip p (rslv s1) (rslv s2)


{- stage 2 compilation, assumes labels have been resolved -}
compile_stage2 :: [S2Cmd] -> [A.Command Int]
compile_stage2 = map translate
    where trLbl :: S2Lbl -> Int
          trLbl (S2State x) = x
          trLbl (S2Lbl lbl) = error ("Unresolved label: " ++ lbl)

          translate (S2Sense sd s1 s2 cond) = A.Sense sd (trLbl s1) (trLbl s2) cond
          translate (S2Mark m st)           = A.Mark m (trLbl st)
          translate (S2PickUp s1 s2)        = A.PickUp (trLbl s1) (trLbl s2)
          translate (S2Drop st)             = A.Drop (trLbl st)
          translate (S2Turn dir st)         = A.Turn dir (trLbl st)
          translate (S2Move s1 s2)          = A.Move (trLbl s1) (trLbl s2)
          translate (S2Flip p s1 s2)        = A.Flip p (trLbl s1) (trLbl s2)
