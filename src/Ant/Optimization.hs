module Ant.Optimization
  ( Optimization
  , Opt
  , applyOpt
  , duplicateCodeOpt
  , unreachableOpt
  ) where

import           Ant.Monad
import           Ant.Base
import           Control.Lens  hiding (uncons)
import           Control.Monad ((>=>))
import           Data.Foldable
import           Data.List     (uncons, (\\), sort)
import qualified Data.Map      as M
import           Data.Maybe    (maybeToList)
import qualified Data.Set      as S
import qualified Control.Category as C

{--
c::AntM L()->Program L

c = fst.snd.runAntM z
(one,two,three,four,five) = (One,Two,Three,Four,Five)
-- --}

--------------------------------------------------------------------------------
  -- Optimization

-- | An optimization is a transformation Program to Program
newtype Opt l1 l2 = Opt { unOpt :: Program l1 -> Program l2 }

type Optimization l = Opt l l

-- This class give us composition of optimizations
instance C.Category Opt where
  id                = Opt id
  (Opt f) . (Opt g) = Opt (f . g)

-- | Apply an Optimization to a given program
applyOpt :: Optimization l -> Program l -> Program l
applyOpt = unOpt

--------------------------------------------------------------------------------
  -- Unreachable code optimization

unreachableOpt :: Optimization L
unreachableOpt = Opt $ closeGaps . \prog ->
  prog & commands %~ flip (foldl (flip M.delete))
                          (unreachable prog)

-- All labels that are reachable from the entry point.
reachable :: Ord l => Program l -> [l]
reachable prog = reach (S.singleton (prog ^. entry))
                         (prog ^. entry)
  where
    reach vs l =
      (l:) . concatMap (reach (S.insert l vs))
           . filter (not . flip S.member vs)   . concatMap toList
           . maybeToList . M.lookup l $ (prog ^. commands)

-- | All labels that are not reachable.
unreachable :: Ord l => Program l -> [l]
unreachable prog = M.keys (prog ^. commands) \\ reachable prog


--------------------------------------------------------------------------------
  -- Duplicate code optimization

duplicateCodeOpt :: Optimization L
duplicateCodeOpt = Opt $ renameEntryPoint z . \prog ->
  let (dups, upd) = duplicated prog
   in closeGaps $
      prog & entry    %~ upd
           & commands %~ (M.map (fmap upd)
                         . flip (foldl (flip M.delete)) dups)


-- | Rename entry point
renameEntryPoint :: (Ord l, Label l) => l -> Program l -> Program l
renameEntryPoint new prog =
  let old = prog ^. entry
   in prog & entry .~ new
           & commands %~ (\cmds -> let m = M.map (fmap (\x -> if x==old then new else x)) cmds
                                    in M.insert z (m M.! old) $ M.delete old m)

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

-- | All labels that are duplicated plus a function to update
-- the labels
duplicated :: (Ord l, Label l) => Program l -> ([l], l -> l)
duplicated =
  M.foldl (\(ls,acc) (l,xs)
          -> (xs ++ ls, acc . (\x -> if x `elem` xs then l else x))) ([],id)
  . M.mapMaybe (uncons . sort >=> partial (not . null . snd))
  . M.foldlWithKey (\m k c -> M.insertWith (++) c [k] m) M.empty
  . view commands

-- Renumber states so that they are all adjacent
closeGaps :: Program L -> Program L
closeGaps prog = prog & commands .~ newCmds & entry %~ upd
   where cmds       = prog ^. commands
         lbls       = M.keys cmds
         mapping    = M.fromList $ zip (sort lbls) (map L [0..])
         upd lbl    = M.findWithDefault lbl lbl mapping
         newCmds    = M.mapKeys upd $ M.map (fmap upd) cmds



-- Old code
-- type Command' = Command Int
--Delete duplicates
--Refresh states
--Look for clashing commands

-- removeDubs :: [Command'] -> [Command']
-- removeDubs cmds'@(cmd:cmds) = staging
--     where
--         staging | length cmds' == length newCmds = newCmds
--                 | otherwise = removeDubs newCmds
--         newCmds             = refreshStates cmds' $ cmd:nub cmds


-- refreshStates :: [Command'] -> [Command'] -> [Command']
-- refreshStates cmds new = map (updateStates mapOldToNew) new
--     where cmdSt   = zip cmds [0 .. ]
--           newSt   = zip new [0 .. ]
--           stNew   = zip [0 .. ] new
--           stCmd   = zip [0 .. ] cmds
--           mapOldToNew     = map lookupCmd stCmd
--           lookupCmd (n,c) = (n,lookup' c newSt)



-- updateStates :: Eq k => [(k,a)] -> Command k -> Command a
-- updateStates env = fmap (`lookup'` env)

-- --Need to add staging to the remove deadcode
-- removeDeadCode :: [Command'] -> [Command']
-- removeDeadCode original =
--                       let cmdBools = zip original $ replicate (length original) False
--                           cmds' = notDead cmdBools [0]
--                           notDead' = foldr (\(c,p) b -> if p then c:b else b) [] cmds'
--                       in refreshStates original notDead'
--     where
--     notDead :: [(Command',Bool)] -> [Int] -> [(Command',Bool)]
--     notDead cmds [] = cmds
--     notDead cmds (n:xs) = notDead updatedCmd (collectSt++xs)
--         where collectSt | notPassed = foldMap (:[]) current
--                         | otherwise = []
--               updatedCmd | notPassed = updateList n (current,True) cmds
--                          | otherwise = cmds
--               (current,passed) = cmds !! n
--               notPassed = not passed

-- updateList :: Int -> a -> [a] -> [a]
-- updateList n a env = f $ splitAt n env
--     where f (xs,y:ys) = xs ++ a:ys
--           f _         = error "Index out of bound"



-- lookup' c xs = fromJust $ lookup c xs

-- test xs = zip xs (replicate (length xs) False)

-- test1 :: [Command']
-- test1 = [Move 2 2,Move 0 1, Move 0 3, Move 0 1]

-- test2 :: [Command']
-- test2 = [Move 0 1, Move 0 1]
