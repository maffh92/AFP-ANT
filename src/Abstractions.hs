{-# LANGUAGE RecursiveDo #-}

module Abstractions where
import Control.Monad.Fix
import Control.Monad
import Ant

abstr_test :: (Label l, MonadFix m) => AntT m l ()
abstr_test = mdo
    loop (\cont brk -> mdo
        search ahead food (\exc -> move_ exc >> pickup_ exc)
        search ahead home (\exc -> move_ exc >> drop')
     )
 where

    search dir what whenFound = loop (\continue break -> mdo
        sense dir what (whenFound continue >> break) (moveAnyDir >> continue)
     )

    moveAnyDir = redo (\exc -> choose [turn left, turn right, move_ exc])




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

cont :: (Label l, MonadFix m) => (l -> AntT m l a) -> AntT m l a
cont c = mdo
  a <- c l
  l <- label
  return a

-- | Do nothing.nt
nop :: (Label l, MonadFix m) => AntT m l ()
nop = cont (flip_ 1 . goto)

-- | Do nothing for @n@ times.
nops :: (MonadFix m, Label l)  => Int -> AntT m l ()
nops n = replicateM_ n nop


-- | While condition is true, run code
while :: (MonadFix m, Label l) => SenseTest -> AntT m l a -> AntT m l ()
while cond code = loop (\cont brk -> if' cond (code >> cont) brk)

-- | Investigate the environment and branch accordingly
if' :: (MonadFix m, Label l) => SenseTest -> AntT m l () -> AntT m l () -> AntT m l ()
if' T         t f = t
if' F         t f = f
if' (d :=: s) t f = sense d s t f
if' (a :&: b) t f = optimizeBranches t f (\t' f' -> if' a (if' b t' f') f')
if' (a :|: b) t f = optimizeBranches t f (\t' f' -> if' a t' (if' b t' f'))
if' (Not cnd) t f = if' cnd f t

-- | Generate optimized code for nested branches (note: not reentrant!)
optimizeBranches :: (MonadFix m) => AntT m l a 
                                 -> AntT m l b
                                 -> (AntT m l () -> AntT m l () -> AntT m l c)
                                 -> AntT m l c
optimizeBranches b1 b2 main = mdo
    val <- main (goto b1') (goto b2'); goto out
    b1' <- label; b1; goto out
    b2' <- label; b2; goto out
    out <- label
    return val


