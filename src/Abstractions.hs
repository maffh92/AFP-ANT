{-# LANGUAGE RecursiveDo #-}

module Abstractions where
import Control.Monad.Fix
import Control.Monad
import Ant

abstr_test :: (Label l, MonadFix m) => AntT m l ()
abstr_test = mdo
    loop (\cont brk -> mdo
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
choose :: (Label l, MonadFix m) => [AntT m l a] -> AntT m l ()
choose cmds = loop (\cont brk -> choose' brk cmds >> brk)
 where
    choose' _   []          = return ()
    choose' end [cmd]       = cmd >> end
    choose' end (cmd:cmds)  = flip' (length cmds+1) (cmd >> end) (choose' end cmds)


data SenseTest = SenseDir :=: Condition
               | SenseTest :&: SenseTest
               | SenseTest :|: SenseTest
               | T | F
               | Not SenseTest

infixr 4 :=:
infixr 3 :&:
infixr 2 :|:

-- | Do nothing.
nop :: (Label l, MonadFix m) => AntT m l ()
nop = mdo
  flip'_ 1 (goto next)
  next   <- label
  return ()

-- | Do nothing for @n@ times.
nops :: (MonadFix m, Label l)  => Int -> AntT m l ()
nops n = replicateM_ n nop

-- | While some condition holds execute.
-- while :: (Label l, MonadFix m) => SenseTest -> AntT m l ()   -> AntT m l ()
-- while cond body = mdo
--   l     <- label
--   ifSense cond (goto l) (goto break)
--   break <- label
--   return ()

-- -- |Investigate the environment and branch accordingly
-- ifSense :: (Label l, MonadFix m) => SenseTest -> l -> l -> AntT m l ()
-- ifSense T              ifT ifF = goto ifT
-- ifSense F              ifT ifF = goto ifF
-- ifSense (dir :=: what) ifT ifF = sense dir what ifT ifF
-- ifSense (a   :&: b   ) ifT ifF = optimize2 ifT ifF (\ifT' ifF' -> ifSense a (ifSense b ifT' ifF') ifF')
-- ifSense (a   :|: b   ) ifT ifF = optimize2 ifT ifF (\ifT' ifF' -> ifSense a ifT' (ifSense b ifT' ifF'))



-- -- |Optimalization of a routine with two subroutines.
-- -- |Instead of generating the same code several times, include the code once and generate entry points
-- -- |instead.
-- optimize2 :: (MonadFix m) => AntT m l a
--                           -> AntT m l b
--                           -> (AntT m l () -> AntT m l () -> AntT m l c)
--                           -> AntT m l c
-- optimize2 sub1 sub2 main = mdo
--       val <- main (goto sub1') (goto sub2')
--       goto out


--       sub1' <- label
--       sub1
--       goto out

--       sub2' <- label
--       sub2
--       goto out
     
--       out <- label
--       return val
