{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import Ant
import Abstractions
import Control.Monad.Fix

{- Need to find a way to get these strategies in a list to randomly choose from.
strategies :: (MonadFix m, Label l) => [AntT m l ()]
strategies =
  [ foodTrail               --                l -> AntT m l () -> AntT m l ()

  , returnFood              --                               l -> AntT m l ()
  , clearFoodTrail          --                               l -> AntT m l ()

  , senseFood               -- AntT m l () -> AntT m l () -> l -> AntT m l ()
  , senseForHome            -- AntT m l () -> AntT m l () -> l -> AntT m l ()

  , senseFoodAndFriend      -- AntT m l () -> AntT m l () -> AntT m l () -> AntT m l ()

  , sensemarkFood           -- AntT m l () -> AntT m l () -> AntT m l ()
  , senseFoodTrailAndFriend -- AntT m l () -> AntT m l () -> AntT m l ()
  , senseHome               -- AntT m l () -> AntT m l () -> AntT m l ()
  , nonBlockingMove         -- AntT m l () -> AntT m l () -> AntT m l ()

  , senseHomeMarker         -- AntT m l () -> AntT m l ()
  , senseRock               -- AntT m l () -> AntT m l ()

  , searchFood              -- AntT m l ()
  ]
-}

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
         (randomMove (goto searchFood'))
      $  if' (ahead :=: home)
             (pickup (move drop' $ goto searchFood') (goto searchFood'))
            $ mdo
              searchHome <- label
              while (Not $ here :=: home)
                    $ senseHome drop'
                                (senseHomeMarker (move (goto searchHome)
                                                 (randomMove $ goto searchHome)))
    )
    (
      sensemarkFood (foodtrail searchFood' $ returnFood searchFood')
        $ senseRock (markHome >> randomMove (goto searchFood'))
    )
    searchFood'

senseFood ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    l ->
    AntT m l ()
senseFood _foodFound _nothing _searchFood =
  if' (here :=: food)
      _foodFound
    $ if' (ahead :=: food)
          (unmarkHome >> move _foodFound (goto _searchFood))
        $ if' (rightAhead :=: food)
              (turnTo right $ goto _searchFood)
            $ if' (leftAhead :=: food)
                  (turnTo left $ goto _searchFood)
                  _nothing

sensemarkFood ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
sensemarkFood _foodTrail _nothing =
  if' (ahead :=: foodMarker)
      _foodTrail
    $ if' (rightAhead :=: foodMarker)
          (turnTo right  _foodTrail)
        $ if' (leftAhead :=: foodMarker)
              (turnTo left _foodTrail)
              _nothing

senseRock ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l ()
senseRock _nothing =
  if' (ahead :=: rock)
  (
    if' (rightAhead :=: rock)
        (
          if' (leftAhead :=: rock)
              (turnTo right $ turnAround _nothing)
              (turnTo left $ turnAround _nothing)
        )
      $ turnTo right $ turnAround _nothing
  )
  _nothing


senseFoodAndFriend ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
senseFoodAndFriend _both _food _nothing =
  if' (ahead :=: food)
      (if' (ahead :=: friend) _both _food)
    $ if' (leftAhead :=: food)
          (
            if' (leftAhead :=: friend)
                (move nop nop >> turn left >> _both)
              $ turnTo left _food
          )
        $ if' (rightAhead :=: food)
              (
                if' (rightAhead :=: friend)
                    _both
                  $ turnTo right _food
              )
              _nothing

senseFoodTrailAndFriend ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
senseFoodTrailAndFriend _both _nothing =
  if' (ahead :=: food)
      (if' (ahead :=: friend) _both _both)
      $ if' (leftAhead :=: foodMarker)
            (if' (leftAhead :=: friend)
                  (move nop nop >> turn left >> _both)
                  (turnTo left _both))
            $ if' (rightAhead :=: foodMarker)
                  (
                    if' (rightAhead :=: friend)
                       (move nop nop >> turn right >> _both)
                       (turnTo right _both)
                  )
                  _nothing

senseHome ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
senseHome _home _nothing =
  if' (ahead :=: home)
      _home
      $ if' (leftAhead :=: home)
            (turnTo left _home)
          $ if' (rightAhead :=: home) (turnTo right _home) _nothing

turnTo ::
    (MonadFix m, Label l) =>
    LeftOrRight ->
    AntT m l () ->
    AntT m l ()
turnTo dir to = turn dir >> to

nonBlockingMove ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    AntT m l ()
nonBlockingMove next notpossible =
  senseNo friend next (senseNo rock next notpossible)



senseNo :: (MonadFix m, Label l) => Condition -> AntT m l () ->  AntT m l () -> AntT m l ()
senseNo condition _nothing _notpossible =
  if' (   ahead      :=: condition :|:
          leftAhead  :=: condition :|:
          rightAhead :=: condition
      )
  _notpossible
  _nothing

randomMove ::
  (MonadFix m, Label l) =>
  AntT m l () ->
  AntT m l ()
randomMove _next = flip' 3 (turn left) (turn right) >> move _next _next

turnAround ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l ()
turnAround _next = turn left >> turn left >> _next

dropFood :: (MonadFix m, Label l) => l -> AntT m l ()
dropFood sf = drop' >> turnAround (goto sf)

returnFood :: (MonadFix m, Label l) => l -> AntT m l ()
returnFood _searchFood = mdo
  _returnFood <- label
  senseForHome
    (move (goto _returnFood) (randomMove $ goto _returnFood))
    (senseHomeMarker $ move (goto _returnFood) (randomMove $ goto _returnFood))
    _searchFood

foodtrail :: (MonadFix m, Label l) => l ->  AntT m l () ->  AntT m l ()
foodtrail sf returnFood' = mdo
 ft <- label
 senseFoodAndFriend
 --Sense both friends and food
  (while (ahead :=: friend) nop >> move (goto ft) (randomMove $ goto ft))
  (senseHome
    (turnAround $ goto ft)
    (senseFoodTrailAndFriend
      (while (ahead :=: friend) nop >> move (goto ft) (randomMove $ goto ft))
      (senseNo friend
        (turnAround (clearFoodTrail sf))
        (nonBlockingMove (move (goto sf)
          (randomMove $ goto sf))
          (randomMove $ goto sf)
        )
      )
    )
  )
  (
    move
      (pickup (turnAround returnFood') (turnAround returnFood'))
      (randomMove (goto sf))
  )

clearFoodTrail :: (MonadFix m, Label l) => l -> AntT m l ()
clearFoodTrail sf = mdo
  clearFoodTrail' <- label
  if' (ahead :=: friend)
      (randomMove (goto sf))
      $ if' (ahead :=: foodMarker)
            (
              if' (leftAhead :=: foodMarker)
                  (goto sf)
                $ mdo
                  unmarkFood
                  move (goto clearFoodTrail') (goto sf)
            )
          $ if' (leftAhead :=: foodMarker)
                (
                  if' (rightAhead :=: foodMarker)
                      (goto sf)
                    $ mdo
                      unmarkFood
                      move (turn left) (randomMove $ goto sf)
                      move (goto clearFoodTrail') (randomMove $ goto sf)
                )
              $ mdo
                unmarkFood
                turn left
                move (goto clearFoodTrail') (randomMove $ goto sf)



--For the moment I am not using the following functions:
senseForHome ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l () ->
    l ->
    AntT m l ()
senseForHome next nothing sf =
    if' (here :=: home)
        (dropFood sf)
      $ if' (ahead :=: home)
             next
           $ if' (rightAhead :=: home)
                 (turnTo left next)
               $ if' (leftAhead :=: home) (turnTo left next) nothing

senseHomeMarker ::
    (MonadFix m, Label l) =>
    AntT m l () ->
    AntT m l ()
senseHomeMarker _next =
  if' (here :=: homeMarker)
      (markFood >> _next)
    $ if' (ahead :=: homeMarker)
          (markHome >> _next)
        $ if' (rightAhead :=: homeMarker)
              (turnTo right _next)
           $  if' (leftAhead :=: homeMarker) (turnTo left _next) _next
