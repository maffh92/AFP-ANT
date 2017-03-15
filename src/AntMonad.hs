{-# LANGUAGE RecursiveDo #-}

module AntMonad where
import Control.Monad.Fix
import Control.Monad
import qualified AntState as A
import AntGoto


monad_example :: AntCode ()
monad_example = mdo
    senseFood <- label
    mdo sense A.Ahead food noFood A.Food

    food <- label
    mdo move eat senseFood

    eat <- label
    mdo pickUp senseHome senseFood
   
    noFood <- label
    mdo nflip 3 goLeft rightOrForward

    goLeft <- label
    mdo turn A.Left
        goto senseFood

    rightOrForward <- label
    mdo nflip 2 goRight goForward

    goRight <- label
    mdo turn A.Right
        goto senseFood

    goForward <- label
    mdo move senseFood noFood

    senseHome <- label
    mdo sense A.Ahead home notHome A.Home
    
    home <- label
    mdo move dropFood senseHome

    dropFood <- label
    mdo ndrop
        goto senseFood

    notHome <- label
    mdo nflip 3 hGoLeft hRightOrForward

    hGoLeft <- label
    mdo turn A.Left
        goto senseHome

    hRightOrForward <- label
    mdo nflip 2 hGoRight hGoForward
 
    hGoRight <- label
    mdo turn A.Right
        goto senseHome

    hGoForward <- label
    mdo move senseHome notHome

mcompile :: AntCode () -> String
mcompile = A.compile . compile . ant 0  


newtype AntCode a = AntCode (Int -> ([Command Int], Int, a))

antFn (AntCode fn) = fn
ant start (AntCode fn) = cmds where (cmds, _, _) = fn start

instance Functor AntCode where
    fmap = liftM

instance Applicative AntCode where
    pure = return
    (<*>) = ap

instance Monad AntCode where
    return ret = AntCode $ \loc -> ([], loc, ret)
    a >>= b = AntCode $ \start -> let (left,  mid, val) = antFn a start
                                      (right, end, ret) = antFn (b val) mid
                                  in  (left++right, end, ret)


instance MonadFix AntCode where
    mfix f = AntCode $ \start -> let (c,e,r) = antFn (f r) start in (c,e,r)


data MLbl = MLbl String

label :: AntCode MLbl
label = AntCode $ \loc -> ([Label (show loc)], loc, MLbl (show loc))

goto :: MLbl -> AntCode ()
goto (MLbl label) = AntCode $ \loc -> ([Goto label], loc, ())

mark :: A.Marker -> AntCode ()
mark m = AntCode $ \loc -> ([Mark m], loc+1, ())

unmark :: A.Marker -> AntCode ()
unmark m = AntCode $ \loc -> ([Unmark m], loc+1, ())

ndrop :: AntCode ()
ndrop = AntCode $ \loc -> ([Drop], loc+1, ())

turn :: A.TurnDir -> AntCode ()
turn dir = AntCode $ \loc -> ([Turn dir], loc+1, ())

pickUp :: MLbl -> MLbl -> AntCode ()
pickUp (MLbl s1) (MLbl s2) = AntCode $ \loc -> ([PickUp s1 s2], loc+1, ())

sense :: A.SenseDir -> MLbl -> MLbl -> A.Condition -> AntCode () 
sense sd (MLbl s1) (MLbl s2) cond = AntCode $ \loc -> ([Sense sd s1 s2 cond], loc+1, ())

move :: MLbl -> MLbl -> AntCode ()
move (MLbl s1) (MLbl s2) = AntCode $ \loc -> ([Move s1 s2], loc+1, ())

nflip :: Int -> MLbl -> MLbl -> AntCode ()
nflip p (MLbl s1) (MLbl s2) = AntCode $ \loc -> ([Flip p s1 s2], loc+1, ())


