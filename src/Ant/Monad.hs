{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TemplateHaskell            #-}
module Ant.Monad
  (
  -- * Monad
    AntT
  , AntM
  , runAntT
  , runAntM
  -- ** Basic combinators
  , label
  , goto
  -- ** Non branching commands
  , mark
  , unmark
  , drop'
  , turn
  -- ** Branching commands.
  , move
  , flip'
  , sense
  , pickup
  -- ** Continuous version of Branching commands.
  , move_
  , flip_
  , sense_
  , pickup_
  -- * Program
  , Program
  , commands
  , entry
  , valid
  , size
  -- * Labels
  , Label(..)
  , L(..)
  ) where

import           Ant.Base

import           Control.Lens               hiding (at)
import           Control.Monad.Fix
import           Control.Monad.Tardis.Class
import           Control.Monad.Trans.Tardis (TardisT, runTardisT)
import           Data.Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as M (empty, insert, keys, keysSet,
                                                  lookup)
import           Data.Maybe                 (isJust)
import qualified Data.Set                   as S (fromList, (\\))

--------------------------------------------------------------------------------
-- Label

-- | The class of Labels.
class Ord l => Label l where
  z :: l
  s :: l -> l
 

-- | Cannonical implementation of Label.
newtype L = L { _lab :: Int }
  deriving (Eq, Ord)

makeLenses ''L

instance Label L where
  z = L 0
  s = lab %~ (+1)

instance Show L where
  show = show . view lab



--------------------------------------------------------------------------------
-- Progam

-- | A Program
data Program l =
  Program { _entry     :: l
          , _commands  :: Map l (Command l) }
          deriving (Eq, Show)

makeLenses ''Program

-- | A program is valid iff all labels contained in the commands
-- are keys of the Map.
valid :: (Ord l, Label l) => Program l -> Bool
valid prog =
  let m = prog ^. commands
   in null (S.fromList (foldMap toList m) S.\\ M.keysSet m) &&
      isJust (M.lookup (prog ^. entry) m) 

-- | The size of a program is the number of states
size :: Ord l => Program l -> Int
size  = length . M.keys . view commands

--------------------------------------------------------------------------------
-- Monad

-- | AntT monad transformer is parametrized by the underlying monad @m@,
-- and the type of labels @l@.
newtype AntT m l a =
  AntT { runAnt :: TardisT (Program l) l m a
         -- ^ A AntT is a TardisT monad where the `forward-traveling`
         -- state is the labels, and the `backward-traveling` state is
         -- a Program.
       } deriving (Functor, Applicative, Monad
                  , MonadFix, MonadTardis (Program l) l)


-- | 'AntM' is a specialized version of 'AntT' to the Identity monad.
type AntM l a = AntT Identity l a


-- | Run an 'AntT' computation given the initial label @l@.
runAntT :: l -> AntT m l a -> m (a, (Program l, l))
runAntT i = (`runTardisT` (Program i M.empty, i)) . runAnt

-- | Specialized version of 'runAntT' to 'Identity' monad.
runAntM :: l -> AntT Identity l a -> (a, (Program l, l))
runAntM i = runIdentity . runAntT i

-- | Get the label at the current point.
label :: MonadFix m => AntT m l l
label = view entry <$> getFuture

-- | Set the label at the current point.
goto :: MonadFix m => l -> AntT m l ()
goto l = modifyBackwards (set entry l)

-- | Add a new command to the Program.
-- The `forward` state gets updated, and the cmommand added to the map
-- with the index being the current `forward` state.
addCmd :: (MonadFix m, Label l)
       => Command l -> AntT m l ()
addCmd cmd = do
  i <- getPast
  modifyForwards  s
  modifyBackwards (\prog -> prog & commands %~ M.insert i cmd
                                 & entry .~ i)


-- | Given a 'Command' and branches in case of succees or failure
branchingCmd :: (MonadFix m, Label l)
             => (l -> l -> Command l)
             -> AntT m l a -- ^ Computation in case of success
             -> AntT m l a -- ^ Computation in case of failure
             -> AntT m l ()
branchingCmd cmd success failed = mdo
  addCmd (cmd sucessLabel failedLabel)
  sucessLabel <- label <* success <* goto next
  failedLabel <- label <* failed
  next        <- label
  return ()

-- | A command that continues is a special case of a branching
-- command.
singleCmd :: (MonadFix m, Label l)
           => (l -> Command l)
           -> AntT m l ()
singleCmd cmd =
  branchingCmd (const . cmd) (return ()) (return ())

noBranchingCmd :: (MonadFix m, Label l)
               => (l -> l -> Command l)
               -> AntT m l ()
               -> AntT m l ()
noBranchingCmd cmd = branchingCmd cmd (return ())

--------------------------------------------------------------------------------
-- Non branching commands

mark :: (MonadFix m, Label l) => Marker -> AntT m l ()
mark = singleCmd .  Mark

unmark :: (MonadFix m, Label l) => Marker -> AntT m l ()
unmark = singleCmd . Unmark

drop' :: (MonadFix m, Label l) => AntT m l ()
drop' = singleCmd Drop

turn :: (MonadFix m, Label l) => TurnDir -> AntT m l ()
turn = singleCmd . Turn

--------------------------------------------------------------------------------
-- Branching commands

move :: (MonadFix m, Label l)
     => AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
move = branchingCmd Move

pickup :: (MonadFix m, Label l)
       => AntT m l () -- ^ Success
       -> AntT m l () -- ^ Failure
       -> AntT m l ()
pickup = branchingCmd PickUp

sense :: (MonadFix m, Label l)
     => SenseDir
     -> Condition
     -> AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
sense c s' = branchingCmd (\su fa -> Sense c su fa s')

flip' :: (MonadFix m, Label l)
     => Int
     -> AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
flip' n = branchingCmd (Flip n)

--------------------------------------------------------------------------------
-- Branching commands that only branch in one argument.
-- Only branch in case of failure.

move_ :: (MonadFix m, Label l)
      => AntT m l () -- ^ Failure
      -> AntT m l ()
move_ = noBranchingCmd Move
pickup_ :: (MonadFix m, Label l)
        => AntT m l () -- ^ Failure
        -> AntT m l ()
pickup_ = noBranchingCmd PickUp

sense_ :: (MonadFix m, Label l)
       => SenseDir
       -> Condition
       -> AntT m l () -- ^ Failure
       -> AntT m l ()
sense_ c s' = noBranchingCmd (\su fa -> Sense c su fa s')

flip_ :: (MonadFix m, Label l)
      => Int
      -> AntT m l () -- ^ Failure
      -> AntT m l ()
flip_ n = noBranchingCmd (Flip n)
