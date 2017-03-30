{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import Ant
import Abstractions
import Control.Monad.Fix


-- markFood,homeMarker :: Marker
markFood :: (MonadFix m, Label l) => AntT m l ()
markFood = mark zero
unmarkFood :: (MonadFix m, Label l) => AntT m l ()
unmarkFood = unmark zero

foodMarker :: Condition
foodMarker = marker zero

markHome :: (MonadFix m, Label l) => AntT m l ()
markHome = mark one

homeMarker :: Condition
homeMarker = marker one

unmarkHome :: (MonadFix m, Label l) => AntT m l ()
unmarkHome = unmark one

searchFood ::
    (MonadFix m, Label l) =>
    AntT m l ()
searchFood =
  mdo
  searchFood' <- label
  senseFood
     (if' (here :=: home)
         (randomMove (goto searchFood') searchFood')
      $  if' (ahead :=: home)
             (move (randomMove (goto searchFood') searchFood') (move (goto searchFood') (goto searchFood')))
           $ pickup (turnAround $ goto searchFood') (goto searchFood')
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
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    l ->
    AntT m l ()
senseFood foodFound nothing searchFood' =
  if' (here :=: food) 
      foodFound
    $ if' (ahead :=: food)
          (
            mdo
            markHome
            move foodFound (goto searchFood')
          )
        $ if' (rightAhead :=: food)
              (turnTo right $ goto searchFood')
            $ if' (leftAhead :=: food)
              (turnTo left $ goto searchFood')
              nothing

sensemarkFood ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
sensemarkFood foodTrail' nothing =
  if' (ahead :=: foodMarker) 
      foodTrail'
    $ if' (rightAhead :=: foodMarker)
          (turnTo right foodTrail')
        $ if' (leftAhead :=: foodMarker)
              (turnTo left foodTrail')
              nothing

senseRock ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l ()
senseRock nothing =
  if' (ahead :=: rock)
  (
    if' (rightAhead :=: rock)
        (
          if' (leftAhead :=: rock)
          (turnTo right $ turnAround nothing)
          (turnTo left $ turnAround nothing)
        )
      $ turnTo right $ turnAround nothing
  )
  nothing

senseForHome ::
    (MonadFix m, Label l) =>
    l ->
    l ->
    l ->
    AntT m l ()
senseForHome next nothing sf =
    if' (here :=: home) 
        (dropFood sf)
     $ if' (ahead :=: home) 
           (goto next)
         $ if' (rightAhead :=: home)
               (turnTo left $ goto next)
             $ if' (leftAhead :=: home)
                   (turnTo left $ goto next)
                  $ goto nothing

senseHomeMarker ::
    (MonadFix m, Label l) =>
    l ->
    l ->
    AntT m l ()
senseHomeMarker next returnFood' =
  if' (here :=: homeMarker)
      (
        mdo
        unmarkHome
        markFood
        goto returnFood'
      )
    $ if' (ahead :=: homeMarker) 
          (goto next)
          $ if' (rightAhead :=: homeMarker)
                (turnTo right (goto next))
             $  if' (leftAhead :=: homeMarker)
                (turnTo left (goto next))
              $ randomMove (goto returnFood') returnFood'

senseFoodAndFriend ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
senseFoodAndFriend both food' nothing = 
  if' (ahead :=: food)
      (if' (ahead :=: friend) both food')
    $ if' (leftAhead :=: food)
          (
            if' (leftAhead :=: friend) 
                both
              $ turnTo left food'
          )
        $ if' (rightAhead :=: food)
              (
                if' (rightAhead :=: friend)
                   both
                 $ turnTo right food'
              )
              nothing

senseFoodTrailAndFriend ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l () ->
    AntT m l () 
senseFoodTrailAndFriend both marker' nothing =
  if' (ahead :=: food)
      (if' (ahead :=: friend)  both marker')
      $ if' (leftAhead :=: foodMarker)
            (if' (leftAhead :=: friend) 
                  both 
                  (turnTo left marker'))
            $ if' (rightAhead :=: foodMarker)
                (
                  if' (rightAhead :=: friend)
                     both
                     (turnTo right marker')
                )
                nothing

senseHome ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
senseHome home' nothing =
  if' (ahead :=: home) 
          home' 
          $ if' (leftAhead :=: home)
              (turnTo left home')
              $ if' (rightAhead :=: home)
                (turnTo right home')
                nothing

turnTo :: 
    (MonadFix m, Label l) => 
    LeftOrRight -> 
    AntT m l () -> 
    AntT m l ()
turnTo dir destination = mdo
                         turn dir
                         destination 
    
nonBlockingMove ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
nonBlockingMove next notpossible =
    senseNoFriend next (senseNoRock next notpossible)

senseNoRock,senseNoFriend ::
  (MonadFix m, Label l) =>
  AntT m l () ->
  AntT m l () ->
  AntT m l ()
senseNoRock = senseNo rock
senseNoFriend = senseNo friend


senseNo :: (MonadFix m, Label l) => Condition -> AntT m l () ->  AntT m l () -> AntT m l ()
senseNo condition nothing destination = 
  if' (
          ahead     :=: condition 
      :|: leftAhead :=: condition
      :|: rightAhead :=: condition
      )
  destination
  nothing

randomMove ::
  (MonadFix m, Label l) =>
  AntT m l () ->
  l ->
  AntT m l ()
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
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l ()
turnAround next = mdo
  turn left
  turn left
  next

dropFood :: (MonadFix m, Label l) => l -> AntT m l ()
dropFood sf =
  mdo
  drop'
  turnAround $ goto sf

returnFood :: (MonadFix m, Label l) =>  l -> AntT m l ()
returnFood searchFood' = mdo
  returnFood' <- label
  randomMove
    (move (goto returnFood') (goto returnFood'))
    searchFood'

foodtrail :: (MonadFix m, Label l) => l ->  AntT m l () ->  AntT m l ()
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

clearFoodTrail :: (MonadFix m, Label l) => l -> AntT m l ()
clearFoodTrail sf = mdo
  clearFoodTrail' <- label
  if' (ahead :=: friend)
      (randomMove (goto sf) sf)
      $ if' (ahead :=: foodMarker)
            (
              if' (leftAhead :=: foodMarker) (goto sf)
                $ mdo
                  unmarkFood
                  move (goto clearFoodTrail') (goto clearFoodTrail')
            )
          $ if' (leftAhead :=: foodMarker)
                (
                  if' (rightAhead :=: foodMarker) 
                      (goto sf)
                    $ mdo
                      unmarkFood
                      turn right
                      move (goto clearFoodTrail') (goto clearFoodTrail')
                )
            $ mdo
              unmarkFood
              turn left
              move (goto clearFoodTrail') (goto clearFoodTrail')
      
