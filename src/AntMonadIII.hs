{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecursiveDo #-}
module AntMonadIII where

import           AntState as A
import           Control.Lens hiding (at)
import           Control.Monad.Fix
import           Control.Monad.Trans.Tardis
import Control.Applicative
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import Prelude as P hiding (drop, flip)

data Program a =
  Program { _entry     :: a
          , _commands  :: Map a (Command a) }
          deriving (Eq, Show)

makeLenses ''Program

type AntT m l a = TardisT (Program l) l m a

type AntM l a = AntT Identity l a

runAntT :: l -> AntT m l a -> m (a, (Program l, l))
runAntT i m = runTardisT m (Program i M.empty, i)

runAntM :: l -> AntT Identity l a -> (a, (Program l, l))
runAntM i = runIdentity . runAntT i

genProg :: AntT Identity Int a -> String
genProg = showCmds . M.toList . view commands . fst . snd . runAntM 0

label :: MonadFix m => AntT m l l
label = view entry <$> getFuture

-- | Set the entry point for the program to the given label
goto :: MonadFix m => l -> AntT m l ()
goto l = modifyBackwards (set entry l)


-- Add a new command to the Program.
-- The `forward` state gets updated, and the cmmand added to the map
-- with the index being the current `forward` state.
addCmd :: (MonadFix m, Ord l, Enum l)
       => Command l -> AntT m l ()
addCmd cmd = do
  i <- getPast
  modifyForwards  succ
  modifyBackwards (\prog -> prog & commands %~ M.insert i cmd
                                 & entry .~ i)

branchingCmd :: (MonadFix m, Ord l, Enum l)
             => (l -> l -> Command l)
             -> AntT m l a
             -> AntT m l a
             -> AntT m l ()
branchingCmd cmd success failed = mdo
  addCmd (cmd sucessLabel failedLabel)
  sucessLabel <- label <* success <* goto next
  failedLabel <- label <* failed  <* goto next
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

drop :: (MonadFix m, Ord l, Enum l) => AntT m l ()
drop = singleCmd Drop

turn :: (MonadFix m, Ord l, Enum l) => TurnDir -> AntT m l ()
turn = singleCmd . Turn

-- Branching commands
move :: (MonadFix m, Ord l, Enum l)
     => AntT m l ()
     -> AntT m l ()
     -> AntT m l ()
move = branchingCmd Move

pickup :: (MonadFix m, Ord l, Enum l)
       => AntT m l ()
       -> AntT m l ()
       -> AntT m l ()
pickup = branchingCmd PickUp

sense :: (MonadFix m, Ord l, Enum l)
     => SenseDir
     -> Condition
     -> AntT m l ()
     -> AntT m l ()
     -> AntT m l ()
sense c s = branchingCmd (\su fa -> Sense c su fa s)

flip :: (MonadFix m, Ord l, Enum l)
     => Int
     -> AntT m l ()
     -> AntT m l ()
     -> AntT m l ()
flip n = branchingCmd (Flip n)

-- No branching version of branching commands
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

flip_ :: (MonadFix m, Ord l, Enum l)
      => Int
      -> AntT m l ()
      -> AntT m l ()
flip_ n = noBranchingCmd (Flip n)


monad_test :: (MonadFix m, Enum l, Ord l) => AntT m l ()
monad_test = mdo
    senseFood <- label
    sense A.Ahead A.Food
      (mdo
        -- there is food.
        -- move, go back if fails
        move_   (goto senseFood)
        -- pick up the food
        pickup_ (goto senseFood)

        -- go home
        goto senseHome;
      )
      (mdo
        -- there is no food, so either go left,
        noFood <- label

        flip 3 (turn A.Left)
            -- or go right,
            (flip 2 (turn A.Right)
               -- or go forward
               (move_ (goto noFood)))
        goto senseFood
     )

    senseHome <- label
    sense A.Ahead A.Home (mdo {
        -- we are home!
        move_ (goto senseHome);

        -- drop the food and go search for more
        drop;
        goto senseFood;

     }) (mdo {
        -- we are not home, so either go left,
        notHome <- label;
        flip 3 (turn A.Left) (mdo
           -- or go right,
           flip 2 (turn A.Right) (mdo
              -- or go forward
              move_ (goto notHome)
            )
        );
        goto senseHome;
     })

