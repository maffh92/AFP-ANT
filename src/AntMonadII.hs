{-# LANGUAGE RecursiveDo #-}

module AntMonadII where

import Control.Monad.Fix
import Control.Monad
import qualified AntState as A
import AntGoto

monad_test2 = mdo
        
        senseFood <- label
        search A.Ahead A.Food (\begin -> mdo {
            -- found food, move onto it if possible
            move (goto begin); 
            -- pick up the food
            pickUp (goto begin); 
            -- find home
            goto senseHome; 
        })

 
        senseHome <- label
        search A.Ahead A.Home (\begin -> mdo {
            -- found home
            move (goto begin); 
            -- drop the food and go search for more
            ndrop;
            goto senseFood;
        })
  
        goto senseFood

  where -- search for something, and run given commands if possible
        search dir what whenFound = mdo
            searchStart <- label
            sense dir what (mdo {
                -- found it
                whenFound searchStart
            }) (mdo {
                -- didn't find it, search onwards
                moveAnyDirection;
                goto searchStart;
            })        

        -- move in any of the possible direction
        moveAnyDirection = mdo 
            retry <- label
            -- either go left
            nflip 3 (turn A.Left) (mdo
                -- or go right
                nflip 2 (turn A.Right) (mdo
                   -- or go forward
                   move (goto retry)
                 )
             )

        

        

monad_test = mdo
    senseFood <- label
    sense A.Ahead A.Food (mdo {
        -- there is food.
        -- move, go back if fails
        move (goto senseFood);
       
        -- pick up the food
        pickUp (goto senseFood);

        -- go home
        goto senseHome;
     }) (mdo {
        -- there is no food, so either go left,
        noFood <- label;

        nflip 3 (turn A.Left) (mdo
            -- or go right,
            nflip 2 (turn A.Right) (mdo
               -- or go forward
               move (goto noFood)
             )
        );
        goto senseFood;
     })

    senseHome <- label
    sense A.Ahead A.Home (mdo {
        -- we are home!
        move (goto senseHome);
        
        -- drop the food and go search for more
        ndrop;
        goto senseFood;
 
     }) (mdo {
        -- we are not home, so either go left,
        notHome <- label;
        nflip 3 (turn A.Left) (mdo
           -- or go right,
           nflip 2 (turn A.Right) (mdo
              -- or go forward
              move (goto notHome)
            )
        );
        goto senseHome;
     })

    goto senseFood -- go back to the beginning, why not.


     

mcompile :: AntCode a -> String
mcompile = A.compile . compile . ant 0

--                                          commands   location label? ret
newtype AntCode a = AntCode ((Int) -> ([Command], Int,  a))

antFn (AntCode fn) = fn
ant start (AntCode fn) = cmds where (cmds, _, _) = fn start

instance Functor AntCode where fmap = liftM
instance Applicative AntCode where pure = return; (<*>) = ap

instance Monad AntCode where
    return ret = AntCode $ \(loc) -> ([], loc, ret)
    a >>= b    = AntCode $ \(start) -> let (left,  mid,  val) = antFn (a) start
                                           (right, end,  ret) = antFn (b val) mid
                                       in  (left++right, end, ret)

instance MonadFix AntCode where
    mfix f = AntCode $ \(start) -> let (c,e,r) = antFn (f r) start in (c,e,r)

data MLbl = MLbl String

-- wrap a list of underlying commands in the monad
wrapCmds :: [Command] -> AntCode MLbl
wrapCmds cmds = AntCode $ \(loc) -> let loc' = loc+length cmds in (cmds, loc', MLbl (show loc'))


label :: AntCode MLbl
label = AntCode $ \(loc) -> ([Label (show loc)], loc, MLbl (show loc))

goto :: MLbl -> AntCode ()
goto (MLbl label) = AntCode $ \(loc) -> ([Goto label], loc+1, ())

mark :: A.Marker -> AntCode MLbl
mark m = wrapCmds [Mark m]

unmark :: A.Marker -> AntCode MLbl
unmark m = wrapCmds [Unmark m]

ndrop :: AntCode MLbl
ndrop = wrapCmds [Drop]

turn :: A.TurnDir -> AntCode MLbl
turn dir = wrapCmds [Turn dir]


-- an "error" command - wrap a command with an OK state and an error state
-- so that it uses a block for the error state, and falls through
errCmd :: (MLbl -> MLbl -> AntCode MLbl) -> AntCode a -> AntCode MLbl
errCmd realCmd errBlock = mdo
   begin <- label
   realCmd okState errState
   errState <- label
   errBlock
   okState <- label
   return begin

-- a branching command
branchCmd :: (MLbl -> MLbl -> AntCode MLbl) -> AntCode a -> AntCode b -> AntCode MLbl
branchCmd realCmd blockOne blockTwo = mdo
   begin <- label
   realCmd b1entry b2entry
   
   b1entry <- label
   blockOne
   goto out

   b2entry <- label
   blockTwo
   
   out <- label
   return begin

-- "underlying" pickUp (with two labels)
-- the idea would be we don't export this
pickUp' :: MLbl -> MLbl -> AntCode MLbl
pickUp' (MLbl s1) (MLbl s2) = wrapCmds [PickUp s1 s2]

-- pickUp with only the "error" jump
pickUp :: AntCode a -> AntCode MLbl
pickUp = errCmd pickUp' 

-- underlying move-forward
move' :: MLbl -> MLbl -> AntCode MLbl
move' (MLbl s1) (MLbl s2) = wrapCmds [Move s1 s2]

-- move forward
move :: AntCode a -> AntCode MLbl
move = errCmd move'

-- underlying flip
nflip' :: Int -> MLbl -> MLbl -> AntCode MLbl
nflip' p (MLbl s1) (MLbl s2) = wrapCmds [Flip p s1 s2]

-- flip
nflip :: Int -> AntCode a -> AntCode b -> AntCode MLbl
nflip p = branchCmd (nflip' p)

-- underlying sense
sense' :: A.SenseDir -> MLbl -> MLbl -> A.Condition -> AntCode MLbl
sense' sd (MLbl s1) (MLbl s2) cond = wrapCmds [Sense sd s1 s2 cond]

-- sense
sense :: A.SenseDir -> A.Condition -> AntCode a -> AntCode b -> AntCode MLbl
sense sd cond = branchCmd (\b1 b2 -> sense' sd b1 b2 cond)


