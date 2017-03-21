{-# LANGUAGE RecursiveDo #-}

module Abstractions where
import Control.Monad.Fix
import Ant.Base
import Ant.Monad

abstr_test :: (Enum l, Ord l, MonadFix m) => AntT m l ()
abstr_test = 
    loop (\cont brk -> do
        search Ahead Food (\exc -> move_ exc >> pickup_ exc)
        search Ahead Home (\exc -> move_ exc >> drop')
     )
 where

    search dir what whenFound = loop (\continue break -> mdo
        sense dir what (whenFound continue >> break) (moveAnyDir >> continue)
     )

    moveAnyDir = redo (\exc -> choose [turn DLeft, turn DRight, move_ exc])




-- |Loop. Inner program gets two arguments, the first one is a 'continue' command
-- |and the second one is a 'break' command.
loop :: MonadFix m => (AntT m l () -> AntT m l () -> AntT m l a) -> AntT m l ()
loop cmds = mdo 
    cont <- label
    cmds (goto cont) (goto brk)
    goto cont
    brk <- label
    return ()


-- |Run a program that can fail repeatedly until it succeeds
redo :: MonadFix m => (AntT m l () -> AntT m l a) -> AntT m l ()
redo cmd = loop (\cont brk -> cmd cont >> brk)

-- |Select one of the programs uniformly at random and run it
choose :: (Enum l, Ord l, MonadFix m) => [AntT m l a] -> AntT m l ()
choose cmds = loop (\cont brk -> choose' brk cmds >> brk)
 where
    choose' _   []          = return ()
    choose' end [cmd]       = cmd >> end
    choose' end (cmd:cmds)  = flip' (length cmds+1) (cmd >> end) (choose' end cmds)  


data SenseTest = SenseDir :=: Condition
               | SenseTest :&: SenseTest
               | SenseTest :|: SenseTest
               | T | F


