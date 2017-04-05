{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import           Abstractions
import           Ant
import           Control.Monad.Fix

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

strategy :: (MonadFix m, Label l) => AntT m l ()
strategy =
  frequency [ (8, forever searchForFood )
            , (2, highWayMakers)]

highWayMakers :: (MonadFix m, Label l)
         => AntT m l ()
highWayMakers =
  choose [ makeHighway zero rock home []
         , makeHighway zero rock home [left, left, left]
         , makeHighway one rock home [right]
         , makeHighway one rock home [left, left]
         , makeHighway two rock home [right, right]
         , makeHighway two rock home [left]
         ]

killer :: (MonadFix m, Label l) => AntT m l ()
killer =
  search Nothing foe

bringFood :: (MonadFix m, Label l) => AntT m l ()
bringFood = do
  searchForFood
  pickup_ searchForFood
  for 3 $ turn left
  search  (Just zero) home
  moveAs home
  drop'
  for 2 $ turn left
  redo move_
  followTrail

moveAs :: (MonadFix m, Label l)
       => Condition
       -> AntT m l ()
moveAs c =
  doOnTheDir c
    (redo move_)
    (cross left right)
    (cross right left)
    where
      cross d1 d2 = do turn d1
                       redo move_
                       turn d2
                       redo move_

guardFood :: (MonadFix m, Label l) => AntT m l ()
guardFood = do
  mark one
  redo move_
  search Nothing (marker one)


makeHighway :: (MonadFix m, Label l)
            => Marker
            -> Condition
            -> Condition
            -> [LeftOrRight]
            -> AntT m l ()
makeHighway m c1 c2 dirs = do
  mapM_ turn dirs
  forever $ do
    doWhile c1
    turnAll
    doWhile c2
    turnAll
    where
      doWhile c =
        while (Not (ahead :=: c) :&: Not (ahead :=: friend)) $ do
          move_ avoid
          mark m

turnAll :: (MonadFix m, Label l) => AntT m l ()
turnAll = for 3 (turn left)

avoid :: (MonadFix m, Label l) => AntT m l ()
avoid = do
  l <- label
  choose [escape l left right, escape l right left, nop]
  where
    escape l d1 d2 = do
      turn d1
      move (turnAll >> move_ (goto l) >> turnAll >> turn d2)
           (turn d2)


searchForFood :: (MonadFix m, Label l) => AntT m l ()
searchForFood = do
  s <- label
  search Nothing food
  moveAs food
  pickup_ (goto s)

  while (Not (   ahead :=: marker zero
             :|: ahead :=: marker one
             :|: ahead :=: marker two)) $ do
    try 2 move turnRandom
    flip_ 20 turnRandom

  caseM [ (ahead :=: marker zero, if' (rightAhead :=: marker zero)
                                      (redo move_ >> turn left)
                                      (redo move_ >> turn right))
        , (ahead :=: marker one, if' (rightAhead :=: marker one)
                                      (redo move_ >> turn left)
                                      (redo move_ >> turn right))
        , (ahead :=: marker two, if' (rightAhead :=: marker two)
                                      (redo move_ >> turn left)
                                      (redo move_ >> turn right))]

  l <- label <* while (Not (ahead :=: rock) :&: Not (ahead :=: home))
                      (move_ avoid)

  if' (ahead :=: home)
      (move_ avoid >> drop')
      (move_ avoid >> turnAll >> goto l)

guardHome :: (MonadFix m, Label l) => AntT m l ()
guardHome = do
  search Nothing rock
  search Nothing home
  forever $ do
    mark one
    choose [twice left, twice right]
    search Nothing (marker one)
      where twice d = turn d >> turn d

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

searchForFood2 :: (MonadFix m, Label l) => l -> AntT m l ()
searchForFood2 l = do
  while (Not (ahead :=: food :|: leftAhead :=: food :|: rightAhead :=: food))
        (
          if' (Not (ahead :=: (marker zero) :|: leftAhead :=: (marker zero) :|: rightAhead :=: (marker zero)))
              (
                do
                try 2 move turnRandom
                flip_ 15 turnRandom
              )
              (goto l)
        )
  doOnTheDir food
    (redo move_)
    (cross left right)
    (cross right left)


cross :: (MonadFix m, Label l) => LeftOrRight -> LeftOrRight -> AntT m l ()
cross d1 d2 = do turn d1
                 redo move_
                 turn d2
                 redo move_


followTrail :: (MonadFix m, Label l) => AntT m l ()
followTrail = do
  _trail <- label
  while ((ahead :=: marker zero) :|: (leftAhead :=: marker zero)  :|: (rightAhead :=: marker zero)) 
        (doOnTheDir (marker zero)
                           (redo move_)
                           (cross left right)
                           (cross right left))
  if' (ahead :=: food)
      bringFood
    $ if' (ahead :=: home) 
          ((for 2 $ turn left) >> goto _trail)
          ((for 2 $ turn left) >> clearTrail zero >> searchForFood)


clearTrail :: (MonadFix m, Label l) => Marker -> AntT m l ()
clearTrail m  = do
  while ((ahead :=: (marker m)) :|: (leftAhead :=: (marker m))  :|: (rightAhead :=: (marker m))) 
        ( do
          unmark m
          doOnTheDir (marker m)
                     (redo move_)
                     (cross left right)
                     (cross right left)
        )



--The start is the initial function of the algorithm.
start ::
    (MonadFix m, Label l) =>
    AntT m l ()
start = mdo
  flip' 10 (goto _searchFood) guard --This line is supposed to divide the work between the ants, but it does not work.
  move (goto _searchFood) (goto _searchFood)
  _searchFood  <- label
  while (Not (here :=: food)) (findFood _searchFood)
  if' (here :=: home)
       ( mdo --At this point you found food, but when you at your home it does not make sense to use pickup.
        turn left
        turn left
        move (goto _searchFood) (randomMove $ goto _searchFood)
      )
      ( mdo
        pickup (findMarker2 _pickup) (findMarker2 _pickup)

        _pickup <- label
        -- while (Not $ ahead :=: (marker two)) (turn left)
        unmark two --Mark two is a special mark, which is placed behind the ant before this step.
        move (goto _findHome) (goto _findHome)
      )
  _findHome <- label
  while (Not $ here :=: home) (findHome _findHome)
  drop'
  _searchFoodmarker <- label
  while (Not $ ahead :=: foodMarker) (turn left)
  if' (ahead :=: foodMarker)
      (move (goto _searchFood) (goto _searchFood))
      (goto _searchFoodmarker)

--The guard function is supposed to guard the border of the base. However it's not working.
guard :: (MonadFix m, Label l) => AntT m l ()
guard = mdo
  while (ahead :=: home) (move nop nop)
  while (ahead :=: marker four) nop


findFood :: (MonadFix m, Label l) =>  l -> AntT m l ()
findFood l =
    if' (ahead :=: food)
        (mark two >> (move (goto l) (randomMove $ goto l)))
      $ if' (rightAhead :=: food)
            (move (turn right) (randomMove $ goto l))
          $ if' (leftAhead :=: food)
                (move (turn left) (randomMove $ goto l))
              $ findTrail l


findTrail :: (MonadFix m, Label l) => l -> AntT m l ()
findTrail l =
  if' (ahead :=: foodMarker)
      (markHome >> (move (goto l) (randomMove $ goto l)))
    $ if' (rightAhead :=: foodMarker)
          (move (turn right) (turn right))
        $ if' (leftAhead :=: foodMarker)
              (move (turn left) (turn left))
              (move (goto l) (randomMove $ goto l))


findHome :: (MonadFix m, Label l) => l -> AntT m l ()
findHome l =
      if' (ahead :=: home)
          (markFood >> (move (goto l) (goto l)))
        $ if' (rightAhead :=: home)
              (markFood >> (move (turn right) (turn right)))
            $ if' (leftAhead :=: home)
                  (markFood >> (move (turn left) (turn left)))
                $ findHomeMarker l


findHomeMarker :: (MonadFix m, Label l) => l -> AntT m l ()
findHomeMarker l = mdo
  markFood
  if' (here :=: homeMarker)
      unmarkHome
      $ if' (ahead :=: homeMarker)
          ((move (goto l) (goto l)))
        $ if' (rightAhead :=: homeMarker)
               (move (turn right) (turn right))
          $ if' (leftAhead :=: homeMarker)
                (move (turn left) (turn left))
                (move (goto l) (goto l))


--Marker 2 is placed when you find food in a regular way. It could also happen that you kill an ant.
--In this case you should just loook for the closest home marker and return to the base
findMarker2 :: (MonadFix m, Label l) => l -> AntT m l ()
findMarker2 l = mdo
  if' (ahead :=: marker two) (move (goto l) (goto l)) (turn left)
  if' (ahead :=: marker two) (move (goto l) (goto l)) (turn left)
  if' (ahead :=: marker two) (move (goto l) (goto l)) (turn left)
  if' (ahead :=: marker two) (move (goto l) (goto l)) (turn right >> findHomeMarker l)

dropFood' :: (MonadFix m, Label l) => l -> AntT m l ()
dropFood' l = mdo
  drop'
  turn left
  turn left
  goto l

randomMove ::
  (MonadFix m, Label l) =>
  AntT m l () ->
  AntT m l ()
randomMove _next = flip' 3 (turn left) (turn right) >> move _next _next

--The below functions are not used anymore. It was part of the old strategy. Both of the strategy actaully do not work.
-- -----------------------------------------------
-- searchFood ::
--     (MonadFix m, Label l) =>
--     AntT m l ()
-- searchFood =
--   mdo
--   searchFood' <- label
--   senseFood
--      (if' (here :=: home)
--          (randomMove (goto searchFood'))
--       $  if' (ahead :=: home)
--              (pickup (move drop' $ goto searchFood') (goto searchFood'))
--             $ mdo
--               searchHome <- label
--               while (Not $ here :=: home)
--                     $ senseHome drop'
--                                 (senseHomeMarker (move (goto searchHome)
--                                                  (randomMove $ goto searchHome)))
--     )
--     (
--       sensemarkFood (foodtrail searchFood' $ returnFood searchFood')
--         $ senseRock (markHome >> randomMove (goto searchFood'))
--     )
--     searchFood'

-- senseFood ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     l ->
--     AntT m l ()
-- senseFood _foodFound _nothing _searchFood =
--   if' (here :=: food)
--       _foodFound
--     $ if' (ahead :=: food)
--           (unmarkHome >> move _foodFound (goto _searchFood))
--         $ if' (rightAhead :=: food)
--               (turnTo right $ goto _searchFood)
--             $ if' (leftAhead :=: food)
--                   (turnTo left $ goto _searchFood)
--                   _nothing

-- sensemarkFood ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l ()
-- sensemarkFood _foodTrail _nothing =
--   if' (ahead :=: foodMarker)
--       _foodTrail
--     $ if' (rightAhead :=: foodMarker)
--           (turnTo right  _foodTrail)
--         $ if' (leftAhead :=: foodMarker)
--               (turnTo left _foodTrail)
--               _nothing

-- senseRock ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l ()
-- senseRock _nothing =
--   if' (ahead :=: rock)
--   (
--     if' (rightAhead :=: rock)
--         (
--           if' (leftAhead :=: rock)
--               (turnTo right $ turnAround _nothing)
--               (turnTo left $ turnAround _nothing)
--         )
--       $ turnTo right $ turnAround _nothing
--   )
--   _nothing


-- senseFoodAndFriend ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l ()
-- senseFoodAndFriend _both _food _nothing =
--   if' (ahead :=: food)
--       (if' (ahead :=: friend) _both _food)
--     $ if' (leftAhead :=: food)
--           (
--             if' (leftAhead :=: friend)
--                 (move nop nop >> turn left >> _both)
--               $ turnTo left _food
--           )
--         $ if' (rightAhead :=: food)
--               (
--                 if' (rightAhead :=: friend)
--                     _both
--                   $ turnTo right _food
--               )
--               _nothing

-- senseFoodTrailAndFriend ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l ()
-- senseFoodTrailAndFriend _both _nothing =
--   if' (ahead :=: food)
--       (if' (ahead :=: friend) _both _both)
--       $ if' (leftAhead :=: foodMarker)
--             (if' (leftAhead :=: friend)
--                   (move nop nop >> turn left >> _both)
--                   (turnTo left _both))
--             $ if' (rightAhead :=: foodMarker)
--                   (
--                     if' (rightAhead :=: friend)
--                        (move nop nop >> turn right >> _both)
--                        (turnTo right _both)
--                   )
--                   _nothing

-- senseHome ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l ()
-- senseHome _home _nothing =
--   if' (ahead :=: home)
--       _home
--       $ if' (leftAhead :=: home)
--             (turnTo left _home)
--           $ if' (rightAhead :=: home) (turnTo right _home) _nothing

-- turnTo ::
--     (MonadFix m, Label l) =>
--     LeftOrRight ->
--     AntT m l () ->
--     AntT m l ()
-- turnTo dir to = turn dir >> to

-- nonBlockingMove ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     AntT m l ()
-- nonBlockingMove next notpossible =
--   senseNo friend next (senseNo rock next notpossible)



-- senseNo :: (MonadFix m, Label l) => Condition -> AntT m l () ->  AntT m l () -> AntT m l ()
-- senseNo condition _nothing _notpossible =
--   if' (   ahead      :=: condition :|:
--           leftAhead  :=: condition :|:
--           rightAhead :=: condition
--       )
--   _notpossible
--   _nothing

-- randomMove ::
--   (MonadFix m, Label l) =>
--   AntT m l () ->
--   AntT m l ()
-- randomMove _next = flip' 3 (turn left) (turn right) >> move _next _next

-- turnAround ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l ()
-- turnAround _next = turn left >> turn left >> _next

-- dropFood :: (MonadFix m, Label l) => l -> AntT m l ()
-- dropFood sf = drop' >> turnAround (goto sf)

-- returnFood :: (MonadFix m, Label l) => l -> AntT m l ()
-- returnFood _searchFood = mdo
--   _returnFood <- label
--   senseForHome
--     (move (goto _returnFood) (randomMove $ goto _returnFood))
--     (senseHomeMarker $ move (goto _returnFood) (randomMove $ goto _returnFood))
--     _searchFood

-- foodtrail :: (MonadFix m, Label l) => l ->  AntT m l () ->  AntT m l ()
-- foodtrail sf returnFood' = mdo
--  ft <- label
--  senseFoodAndFriend
--  --Sense both friends and food
--   (while (ahead :=: friend) nop >> move (goto ft) (randomMove $ goto ft))
--   (senseHome
--     (turnAround $ goto ft)
--     (senseFoodTrailAndFriend
--       (while (ahead :=: friend) nop >> move (goto ft) (randomMove $ goto ft))
--       (senseNo friend
--         (turnAround (clearFoodTrail sf))
--         (nonBlockingMove (move (goto sf)
--           (randomMove $ goto sf))
--           (randomMove $ goto sf)
--         )
--       )
--     )
--   )
--   (
--     move
--       (pickup (turnAround returnFood') (turnAround returnFood'))
--       (randomMove (goto sf))
--   )

-- clearFoodTrail :: (MonadFix m, Label l) => l -> AntT m l ()
-- clearFoodTrail sf = mdo
--   clearFoodTrail' <- label
--   if' (ahead :=: friend)
--       (randomMove (goto sf))
--       $ if' (ahead :=: foodMarker)
--             (
--               if' (leftAhead :=: foodMarker)
--                   (goto sf)
--                 $ mdo
--                   unmarkFood
--                   move (goto clearFoodTrail') (goto sf)
--             )
--           $ if' (leftAhead :=: foodMarker)
--                 (
--                   if' (rightAhead :=: foodMarker)
--                       (goto sf)
--                     $ mdo
--                       unmarkFood
--                       move (turn left) (randomMove $ goto sf)
--                       move (goto clearFoodTrail') (randomMove $ goto sf)
--                 )
--               $ mdo
--                 unmarkFood
--                 turn left
--                 move (goto clearFoodTrail') (randomMove $ goto sf)



-- --For the moment I am not using the following functions:
-- senseForHome ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l () ->
--     l ->
--     AntT m l ()
-- senseForHome next nothing sf =
--     if' (here :=: home)
--         (dropFood sf)
--       $ if' (ahead :=: home)
--              next
--            $ if' (rightAhead :=: home)
--                  (turnTo left next)
--                $ if' (leftAhead :=: home) (turnTo left next) nothing

-- senseHomeMarker ::
--     (MonadFix m, Label l) =>
--     AntT m l () ->
--     AntT m l ()
-- senseHomeMarker _next =
--   if' (here :=: homeMarker)
--       (markFood >> _next)
--     $ if' (ahead :=: homeMarker)
--           (markHome >> _next)
--         $ if' (rightAhead :=: homeMarker)
--               (turnTo right _next)
--            $  if' (leftAhead :=: homeMarker) (turnTo left _next) _next
