module Optimization where

import           Ant.Base
import           Data.List
import           Data.Maybe

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
updateStates env cmd = fmap (`lookup'` env) cmd

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
        where collectSt | notPassed = foldMap (:[]) $ current
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
