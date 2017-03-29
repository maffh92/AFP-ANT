{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ant.Optimization where

import           Ant.Base
import           Ant.Monad
import           Data.List
import           Data.Maybe

import           Control.Lens hiding (uncons)
import           Data.Foldable
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import Control.Monad 

type Command' = Command Int
--Delete duplicates
--Refresh states
--Look for clashing commands

removeDubs :: [Command'] -> [Command']
removeDubs cmds'@(cmd:cmds) = staging
    where
        staging | length cmds' == length newCmds = newCmds
                | otherwise = removeDubs newCmds
        newCmds             = refreshStates cmds' $ cmd:nub cmds


refreshStates :: [Command'] -> [Command'] -> [Command']
refreshStates cmds new = map (updateStates mapOldToNew) new
    where cmdSt   = zip cmds [0 .. ]
          newSt   = zip new [0 .. ]
          stNew   = zip [0 .. ] new
          stCmd   = zip [0 .. ] cmds
          mapOldToNew     = map lookupCmd stCmd
          lookupCmd (n,c) = (n,lookup' c newSt)



updateStates :: Eq k => [(k,a)] -> Command k -> Command a
updateStates env = fmap (`lookup'` env)

--Need to add staging to the remove deadcode
removeDeadCode :: [Command'] -> [Command']
removeDeadCode original =
                      let cmdBools = zip original $ replicate (length original) False
                          cmds' = notDead cmdBools [0]
                          notDead' = foldr (\(c,p) b -> if p then c:b else b) [] cmds'
                      in refreshStates original notDead'
    where
    notDead :: [(Command',Bool)] -> [Int] -> [(Command',Bool)]
    notDead cmds [] = cmds
    notDead cmds (n:xs) = notDead updatedCmd (collectSt++xs)
        where collectSt | notPassed = foldMap (:[]) current
                        | otherwise = []
              updatedCmd | notPassed = updateList n (current,True) cmds
                         | otherwise = cmds
              (current,passed) = cmds !! n
              notPassed = not passed

updateList :: Int -> a -> [a] -> [a]
updateList n a env = f $ splitAt n env
    where f (xs,y:ys) = xs ++ a:ys
          f _         = error "Index out of bound"



lookup' c xs = fromJust $ lookup c xs

test xs = zip xs (replicate (length xs) False)

test1 :: [Command']
test1 = [Move 2 2,Move 0 1, Move 0 3, Move 0 1]

test2 :: [Command']
test2 = [Move 0 1, Move 0 1]

--------------------------------------------------------------------------------
  -- Unreachable code optimization

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

-- | Remove all unreachable labels from the Program.
unreachableOpt :: Ord l => Program l -> Program l
unreachableOpt prog =
  prog & commands %~ flip (foldl (flip M.delete))
                          (unreachable prog)

--------------------------------------------------------------------------------
  -- Duplicate code optimization

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

duplicateCodeOpt :: Ord l => Program l -> Program l
duplicateCodeOpt prog =
  let (dups, upd) = duplicated prog
   in prog & entry    %~ upd
           & commands %~ (M.map (fmap upd)
                         . flip (foldl (flip M.delete)) dups)

-- | All labels that are duplicated plus a function to update
-- the labels
duplicated :: Ord l => Program l -> ([l], l -> l)
duplicated =
  M.foldl (\(ls,acc) (l,xs)
          -> (xs ++ ls, acc . (\x -> if x `elem` xs then l else x))) ([],id)
  . M.mapMaybe (uncons >=> partial (not . null . snd))
  . M.foldlWithKey (\m k c -> M.insertWith (++) c [k] m) M.empty
  . view commands
