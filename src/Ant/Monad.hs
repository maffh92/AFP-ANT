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
  , flip'_
  , sense_
  , pickup_
  , Program
  , commands
  )where

import           Ant.Base                   as A

import           Control.Applicative
import           Control.Lens               hiding (at)
import           Control.Monad.Fix
import           Control.Monad.Tardis.Class
import           Control.Monad.Trans.Tardis (TardisT, runTardisT)
import           Data.Map                   (Map)
import qualified Data.Map                   as M

-- | A Program
data Program a =
  Program { _entry     :: a
          , _commands  :: Map a (Command a) }
          deriving (Eq, Show)

makeLenses ''Program

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
addCmd :: (MonadFix m, Ord l, Enum l)
       => Command l -> AntT m l ()
addCmd cmd = do
  i <- getPast
  modifyForwards  succ
  modifyBackwards (\prog -> prog & commands %~ M.insert i cmd
                                 & entry .~ i)


-- | Given a 'Command' and branches in case of succees or failure
branchingCmd :: (MonadFix m, Ord l, Enum l)
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
singleCmd :: (MonadFix m, Ord l, Enum l)
           => (l -> Command l)
           -> AntT m l ()
singleCmd cmd =
  branchingCmd (const . cmd) (return ()) (return ())

noBranchingCmd :: (MonadFix m, Ord l, Enum l)
               => (l -> l -> Command l)
               -> AntT m l ()
               -> AntT m l ()
noBranchingCmd cmd = branchingCmd cmd (return ())

mark :: (MonadFix m, Ord l, Enum l) => Marker -> AntT m l ()
mark = singleCmd .  Mark

unmark :: (MonadFix m, Ord l, Enum l) => Marker -> AntT m l ()
unmark = singleCmd . Unmark

drop' :: (MonadFix m, Ord l, Enum l) => AntT m l ()
drop' = singleCmd Drop

turn :: (MonadFix m, Ord l, Enum l) => TurnDir -> AntT m l ()
turn = singleCmd . Turn

-- | Move
move :: (MonadFix m, Ord l, Enum l)
     => AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
move = branchingCmd Move

-- | PickUp
pickup :: (MonadFix m, Ord l, Enum l)
       => AntT m l () -- ^ Success
       -> AntT m l () -- ^ Failure
       -> AntT m l ()
pickup = branchingCmd PickUp

-- | Sense
sense :: (MonadFix m, Ord l, Enum l)
     => SenseDir
     -> Condition
     -> AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
sense c s = branchingCmd (\su fa -> Sense c su fa s)

-- | Flip
flip' :: (MonadFix m, Ord l, Enum l)
     => Int
     -> AntT m l () -- ^ Success
     -> AntT m l () -- ^ Failure
     -> AntT m l ()
flip' n = branchingCmd (Flip n)

pickup_ :: (MonadFix m, Ord l, Enum l)
       => AntT m l ()
       -> AntT m l ()
pickup_ = noBranchingCmd PickUp

move_ :: (MonadFix m, Ord l, Enum l)
      => AntT m l ()
      -> AntT m l ()
move_ = noBranchingCmd Move

sense_ :: (MonadFix m, Ord l, Enum l)
     => SenseDir
     -> Condition
     -> AntT m l ()
     -> AntT m l ()
sense_ c s = noBranchingCmd (\su fa -> Sense c su fa s)

flip'_ :: (MonadFix m, Ord l, Enum l)
      => Int
      -> AntT m l ()
      -> AntT m l ()
flip'_ n = noBranchingCmd (Flip n)