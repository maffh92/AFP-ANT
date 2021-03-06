{-| 
Module: Optimization
Description: Optimizations

This module contains optimizations that can be done on programs.
-}

module Ant.Optimization
  ( Optimization
  , Opt
  , applyOpt
  , duplicateCodeOpt
  , unreachableOpt
  ) where

import           Ant.Monad
import qualified Control.Category as C
import           Control.Lens     hiding (uncons)
import           Control.Monad    ((>=>))
import           Data.Foldable
import           Data.List        (sort, uncons, (\\))
import qualified Data.Map         as M
import           Data.Maybe       (maybeToList)
import qualified Data.Set         as S


--------------------------------------------------------------------------------
  -- Optimization

-- | An optimization is a transformation Program to Program
newtype Opt l1 l2 = Opt { unOpt :: Program l1 -> Program l2 }

-- | An optimization
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

-- | Optimization that removes unreachable code
unreachableOpt :: Label l => Optimization l
unreachableOpt = Opt $ closeGaps . \prog ->
  prog & commands %~ flip (foldl (flip M.delete))
                          (unreachable prog)

-- | Find all labels that are reachable from the entry point.
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

-- | Optimization that removes duplicate code
duplicateCodeOpt :: Label l => Optimization l
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

-- | All labels that are duplicated plus a function to update
-- the labels
duplicated :: (Ord l, Label l) => Program l -> ([l], l -> l)
duplicated =
  M.foldl (\(ls,acc) (l,xs)
          -> (xs ++ ls, acc . (\x -> if x `elem` xs then l else x))) ([],id)
  . M.mapMaybe (uncons . sort >=> partial (not . null . snd))
  . M.foldlWithKey (\m k c -> M.insertWith (++) c [k] m) M.empty
  . view commands

--------------------------------------------------------------------------------
  -- Utils

-- Renumber states so that they are all adjacent
closeGaps :: Label l => Program l -> Program l
closeGaps prog = prog & commands .~ newCmds & entry %~ upd
   where cmds       = prog ^. commands
         lbls       = M.keys cmds
         mapping'   = M.fromList $ zip (sort lbls) (iterate su z)
         upd lbl    = M.findWithDefault lbl lbl mapping'
         newCmds    = M.mapKeys upd $ M.map (fmap upd) cmds

-- | Given a predicate transform a value a into
-- either Nothing (if false) or Just a
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

