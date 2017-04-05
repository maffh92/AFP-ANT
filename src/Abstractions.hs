{-# LANGUAGE RecursiveDo #-}

module Abstractions where

import           Ant
import           Control.Monad hiding (forever)
import           Control.Monad.Fix

-- |Loop. Inner program gets two arguments, the first one is a 'continue' command
-- |and the second one is a 'break' command.
loop :: MonadFix m
     => (AntT m l () -> AntT m l () -> AntT m l ())
     -> AntT m l ()
loop cmd = mdo
    cont <- label
    cmd (goto cont) (goto brk)
    goto cont
    brk <- label
    return ()

-- | Run a program that can fail repeatedly until it succeeds
redo :: MonadFix m => (AntT m l () -> AntT m l a) -> AntT m l ()
redo cmd = loop (\ct brk -> cmd ct >> brk)


for :: (Label l, MonadFix m) => Int -> AntT m l () -> AntT m l ()
for = replicateM_
-- | Always

data SenseTest = SenseDir  :=: Condition
               | SenseTest :&: SenseTest
               | SenseTest :|: SenseTest
               | T | F
               | Not SenseTest

infixr 4 :=:
infixr 3 :&:
infixr 2 :|:

-- | Given a continuation execute it
-- with a label after it.
cont :: (Label l, MonadFix m)
     => (l -> AntT m l a)
     -> AntT m l a
cont c = mdo
  a <- c l
  l <- label
  return a

-- | Do nothing.
nop :: (Label l, MonadFix m) => AntT m l ()
nop = cont (flip_ 1 . goto)

-- | Do nothing for @n@ times.
nops :: (MonadFix m, Label l)  => Int -> AntT m l ()
nops n = for n nop

-- | Do an action forever.
forever :: (Label l, MonadFix m) => AntT m l a -> AntT m l ()
forever = while T

-- | While condition is met, run code
while :: (MonadFix m, Label l)
      => SenseTest
      -> AntT m l a
      -> AntT m l ()
while cond code = loop (\c brk -> if' cond (code >> c) brk)

-- | Look until we can find what we are looking for.
-- leave a mark on the way.
search :: (MonadFix m, Label l)
       => Maybe Marker
       -> Condition
       -> AntT m l ()
search m cond = do
  while (Not (ahead :=: cond :|: leftAhead :=: cond :|: rightAhead :=: cond)) $ do
    try 2 move turnRandom
    maybe (return ()) mark m
    flip_ 15 turnRandom





-- | Try an action @n@ times, and continue.
try :: (MonadFix m, Label l)
    => Int
    -> (AntT m l () -> AntT m l () -> AntT m l ())
    -> AntT m l ()
    -> AntT m l ()
try n m f = mdo
  for n (m (goto l) f)
  l <- label
  return ()

-- | Investigate the environment and branch accordingly
if' :: (MonadFix m, Label l) => SenseTest -> AntT m l () -> AntT m l () -> AntT m l ()
if' T         t _ = t
if' F         _ f = f
if' (d :=: s') t f = sense d s' t f
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
    b1' <- label <* b1 <* goto out
    b2' <- label <* b2 <* goto out
    out <- label
    return val


--------------------------------------------------------------------------------
  -- Conditional combinators

actOnCond :: (MonadFix m, Label l)
          => [(SenseTest , AntT m l ())]
          -> AntT m l ()
actOnCond = foldr (uncurry if') (return ())

doOnTheDir ::(MonadFix m, Label l)
           =>  Condition
           -> AntT m l ()
           -> AntT m l ()
           -> AntT m l ()
           -> AntT m l ()
doOnTheDir cond ah le ri =
  actOnCond [( ahead      :=: cond, ah)
            ,( leftAhead  :=: cond, le)
            ,( rightAhead :=: cond, ri)]

--------------------------------------------------------------------------------
  -- Randomness combinators

-- | Select one of the programs uniformly at random and run it
choose :: (Label l, MonadFix m) => [AntT m l a] -> AntT m l ()
choose cmds = loop (\cont brk -> choose' brk cmds >> brk)
 where
    choose' _   []          = return ()
    choose' end [cmd]       = cmd >> end
    choose' end (cmd:cmds)  = flip' (length cmds+1) (cmd >> end) (choose' end cmds)

-- | Select a program to run according to its total weight
frequency :: (MonadFix m, Label l)
          => [(Int, AntT m l ())]
          -> AntT m l ()
frequency xs =
  choose (concatMap (uncurry replicate) xs)

-- | Turn randomly to either left or right.
turnRandom :: (Label l, MonadFix m) => AntT m l ()
turnRandom = choose [turn left, turn right]

--------------------------------------------------------------------------------
  -- Other combinators

-- | Try to move, if fail do something else
move_ :: (MonadFix m, Label l)
      => AntT m l () -- ^ Failure
      -> AntT m l ()
move_ = move (return ())

-- | Move always
move__ :: (MonadFix m, Label l)
       => AntT m l ()
move__ = move (return ()) (return ())

-- | Try to pickup, if fail do something else
pickup_ :: (MonadFix m, Label l)
        => AntT m l () -- ^ Failure
        -> AntT m l ()
pickup_ = pickup (return ())

-- | Pickup always
pickup__ :: (MonadFix m, Label l)
         => AntT m l ()
pickup__ = pickup (return ()) (return ())

sense_ :: (MonadFix m, Label l)
       => SenseDir
       -> Condition
       -> AntT m l () -- ^ Failure
       -> AntT m l ()
sense_ c s' = sense c s' (return ())

-- | flip and do something or just continue the flow.
flip_ :: (MonadFix m, Label l)
      => Int
      -> AntT m l ()
      -> AntT m l ()
flip_ n m = flip' n m (return ())
