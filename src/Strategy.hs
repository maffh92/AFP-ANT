{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import Ant.Monad
import Ant.Base
import Control.Monad.Fix
-- searchFood = mdo
--  senseHereHome <- label
--  sense Here Home 
--  (
--      mdo 
--      sense Ahead Home
--      (
--          mdo
--              move randomMove searchFood searchFood
--      )
--      (
--          mdo
--              pickup turnAround returnFood searchFood
--      )
--  )
--  (
--      mdo
--          senseFoodMarker foodTrail (senseRock (mark home) undefined)

--  )



senseFood = undefined


senseFoodMarker = undefined

senseRock = undefined


senseForHome = undefined


senseHomeMarker = undefined

senseFoodAndFriend = undefined

senseFoodTrailAndFriend = undefined

senseHome = undefined


nonBlockingMove = undefined


senseNoFriend = undefined

randomMove :: MonadFix m => L -> L -> AntT m L ()
randomMove _next _blocked =
    flip' 3 
    (
        mdo
        turn DRight
        move (goto _blocked) (goto _next)
    )
    (flip' 2 
          ( mdo
            turn DRight
          )
          ( mdo
            move (goto _blocked) (goto _next)
          ))

turnAround = undefined

returnFood = undefined 