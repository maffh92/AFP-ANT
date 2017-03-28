{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import Ant
import Control.Monad.Fix


-- markFood,homeMarker :: Marker
markFood :: MonadFix m => AntT m L ()
markFood = mark zero
unmarkFood :: MonadFix m => AntT m L ()
unmarkFood = unmark zero

foodMarker = marker zero

markHome :: MonadFix m => AntT m L ()
markHome = mark one
homeMarker = marker one
unmarkHome :: MonadFix m => AntT m L ()
unmarkHome = unmark one



searchFood ::
    MonadFix m =>  
    AntT m L ()
searchFood = 
  mdo
  searchFood' <- label
  senseFood 
    (sense here home
      (randomMove (goto searchFood') searchFood')
      (sense ahead home
        (
          move (randomMove (goto searchFood') searchFood') (move (goto searchFood') (goto searchFood'))
        )
        (
          pickup (turnAround $ goto searchFood') (goto searchFood')
        )
      )
    )
    (
      sensemarkFood (foodtrail searchFood'  $ returnFood searchFood')
        $ senseRock 
          (
            mdo
            markHome
            randomMove (goto searchFood') searchFood'
          )
    )
    searchFood'


senseFood ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L () ->
    L ->
    AntT m L ()
senseFood foodFound nothing searchFood' = 
  sense here food foodFound
  $ sense ahead food
    (
      mdo
      markHome
      move foodFound (goto searchFood')
    )
    (
      sense rightAhead food
        (
          mdo
          turn right
          goto searchFood'
        )
        $
          sense leftAhead food
          (
            mdo
            turn left
            goto searchFood'
          )
          nothing
        
    )

sensemarkFood ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L () ->
    AntT m L ()
sensemarkFood foodTrail' nothing = 
  sense ahead foodMarker foodTrail'
    $ sense rightAhead foodMarker
      (
        mdo
        turn right
        foodTrail'
      )
      $ sense leftAhead foodMarker
        (
          mdo
          turn left
          foodTrail'
        )
        nothing

senseRock ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L ()
senseRock nothing = 
  sense ahead rock
  (
    sense rightAhead rock
      (sense leftAhead rock
        (
          mdo
          turn right
          turnAround nothing
        )
        $ mdo
          turn left
          turnAround nothing
      )
      $ mdo
        turn right
        turnAround nothing
  )
  nothing

senseForHome ::
    MonadFix m => 
    L -> 
    L ->
    L -> 
    AntT m L ()
senseForHome next nothing sf = 
    sense here home (dropFood sf)
    $ sense ahead home (goto next)
      $ sense rightAhead home
        (
          mdo
          turn right
          goto next
        )
        $ sense leftAhead home
          (
            mdo
            turn left
            goto next
          )
          $ goto nothing


senseHomeMarker ::
    MonadFix m => 
    L -> 
    L -> 
    AntT m L ()
senseHomeMarker next returnFood' = 
  sense here homeMarker
    (
      mdo
      unmarkHome
      markFood
      goto returnFood'
    )
    (
      sense ahead homeMarker (goto next)
        (
          sense rightAhead homeMarker
          ( mdo
            turn right
            goto next
          ) 
          (
            sense leftAhead homeMarker
            (
              mdo
              turn left
              goto next
            )
            $ randomMove (goto returnFood') returnFood'
          )

        ) 


    )

senseFoodAndFriend ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L () -> 
    AntT m L () -> 
    AntT m L ()
senseFoodAndFriend both food' nothing = 
  sense ahead food
    (sense ahead friend both food')
    (
      sense leftAhead food
        (
          sense leftAhead friend both
          $ mdo
            turn left
            food'
        )
        (sense rightAhead food
          (sense rightAhead friend both
            $ mdo
              turn right
              food'
          )
          nothing  
        )
    )

senseFoodTrailAndFriend ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L () ->
    AntT m L () ->
    AntT m L ()
senseFoodTrailAndFriend both marker' nothing = 
  sense ahead food
    (sense ahead friend both marker')
    (
      sense leftAhead foodMarker
        ( sense leftAhead friend both $
          mdo
          turn left
          marker'
        ) $
        sense rightAhead foodMarker
          (
            sense rightAhead friend both
            $ mdo
              turn right
              marker'
          )
          nothing
        

    )

senseHome ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L () -> 
    AntT m L ()
senseHome home' nothing =
  sense ahead home home' $
        sense leftAhead home
          (
            mdo
            turn left
            home'
          )
          (
            sense rightAhead home
              (
                mdo
                turn right
                home'
              )
              nothing
          )

nonBlockingMove :: 
    MonadFix m => 
    AntT m L () -> 
    AntT m L () -> 
    AntT m L ()
nonBlockingMove next notpossible = 
    senseNoFriend next (senseNoRock next notpossible)

senseNoRock,senseNoFriend :: 
  MonadFix m => 
  AntT m L () -> 
  AntT m L () -> 
  AntT m L ()
senseNoRock = senseNo rock 
senseNoFriend = senseNo friend 


senseNo :: MonadFix m => Condition -> AntT m L () ->  AntT m L () -> AntT m L ()
senseNo condition nothing destination  =
    sense ahead condition
      (
        sense leftAhead condition
          (sense rightAhead condition destination nothing)
          nothing
      )
      nothing


randomMove :: 
  MonadFix m => 
  AntT m L () -> 
  L -> 
  AntT m L ()
randomMove _next _blocked =
    flip' 3 
    (
        mdo
        turn left
        move _next (goto _blocked)
    )
    (flip' 2 
      (
        mdo 
        turn right
        move (goto _blocked) _next
      )
      (move (goto _blocked) _next))

turnAround ::
    MonadFix m => 
    AntT m L () -> 
    AntT m L ()
turnAround next = mdo
  turn left
  turn left
  next

dropFood :: MonadFix m => L -> AntT m L ()
dropFood sf =
  mdo
  drop'
  turnAround $ goto sf

returnFood :: MonadFix m =>  L -> AntT m L ()
returnFood searchFood' = mdo
  returnFood' <- label
  randomMove 
    (move (goto returnFood') (goto returnFood'))
    searchFood'

foodtrail :: MonadFix m => L ->  AntT m L () ->  AntT m L ()
foodtrail sf returnFood' = mdo
 ft <- label
 senseFoodAndFriend 
  (nonBlockingMove 
    (move (goto sf) (goto sf))
    (randomMove (goto ft) ft))
  (senseHome
    (turnAround $ goto ft)
    (senseFoodTrailAndFriend
      (nonBlockingMove
        (move (goto sf) (goto sf))
        $ goto ft
      )
      (move (goto ft) (randomMove (goto sf) sf))
      (senseNoFriend
        (turnAround (clearFoodTrail sf))
        (nonBlockingMove
          (move (goto sf) (goto sf))
          (goto ft)
        )
      )
    )
  )
  (
    move
      (pickup (turnAround returnFood') (turnAround returnFood'))
      (randomMove (goto sf) sf)
  )

clearFoodTrail :: MonadFix m => L -> AntT m L ()
clearFoodTrail sf = mdo
  clearFoodTrail' <- label
  sense ahead friend
    (randomMove (goto sf) sf)
    ( sense ahead foodMarker
        (
          sense leftAhead foodMarker (goto sf)
            $ mdo
              unmarkFood
              move (goto clearFoodTrail') (goto clearFoodTrail')
        )
        (
          sense leftAhead foodMarker
            (
              sense rightAhead foodMarker (goto sf)
              $ mdo
                unmarkFood
                turn right
                move (goto clearFoodTrail') (goto clearFoodTrail')
            )
            (
              mdo
              unmarkFood
              turn left
              move (goto clearFoodTrail') (goto clearFoodTrail')
            )
        )
    )