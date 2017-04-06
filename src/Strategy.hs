{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import           Abstractions
import           Ant
import           Control.Monad.Fix

-- The strategy function is used as the intitial start of our strategy.
-- We use 4 different kinds of strategy. The idea is that we choose with a weighted probablity one of these strategy.
-- The strategy with the highest weight it uses a highway to find food and return to th base
-- The second highest just tries to find and return to the base.
-- We have two strategies with the same weight. One of them tries to protect the food base and the other one creates the highway.
strategy :: (MonadFix m, Label l) => AntT m l ()
strategy =
  frequency [ (8, forever searchForFood )
            , (2, highWayMakers)
            , (2, stealFood)
            , (5, bringFood)]

highWayMakers :: (MonadFix m, Label l)
         => AntT m l ()
highWayMakers =
  choose $ map forever 
    [ makeHighway three rock home []
    , makeHighway three rock home [left, left, left]
    , makeHighway one rock home [right]
    , makeHighway one rock home [left, left]
    , makeHighway two rock home [right, right]
    , makeHighway two rock home [left]
    ]

-- Go to the enemies base and steal their food
stealFood :: (MonadFix m, Label l) => AntT m l ()
stealFood = forever $ search Nothing foeHome >> searchForFood

--Tries to find food and bring it to home
bringFood :: (MonadFix m, Label l) => AntT m l ()
bringFood = do
  s <- label
  search Nothing food
  moveAs food
  pickup_ (goto s)
  for 2 $ turn left
  search  (Just zero) home
  moveAs home
  drop'
  for 2 $ turn left
  redo move_
  followTrail s
  goto s

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


searchForFood :: (MonadFix m, Label l) => AntT m l ()
searchForFood = do
  s <- label
  search Nothing food
  moveAs food
  pickup_ (goto s)

  while (Not (   ahead :=: marker three
             :|: ahead :=: marker one
             :|: ahead :=: marker two)) $ do
    try 2 move turnRandom
    flip_ 20 turnRandom

  caseM [ (ahead :=: marker three, if' (rightAhead :=: marker three)
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
      (for 3 (move_ avoid)    >> drop')
      (move_ avoid >> turnAll >> goto l)


--In the initial search for food we do not check for food marker. However, in the second run we also check for marker food
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
  moveAs food


--Followtrail tries to follow some foodtrail
followTrail :: (MonadFix m, Label l) => l -> AntT m l ()
followTrail l = do
  _trail <- label
  while ((ahead :=: marker zero) :|: (leftAhead :=: marker zero)  :|: (rightAhead :=: marker zero)) 
        (moveAs (marker zero))
  if' (ahead :=: food)
      (goto l)
    $ if' (ahead :=: home) 
          ((for 2 $ turn left) >> goto _trail)
          ((for 2 $ turn left) >> clearTrail zero >> searchForFood2 _trail)


--Given some marker, this function will remove the trail
clearTrail :: (MonadFix m, Label l) => Marker -> AntT m l ()
clearTrail m  = do
  while ((ahead :=: (marker m)) :|: (leftAhead :=: (marker m))  :|: (rightAhead :=: (marker m))) 
        ( do
          unmark m
          moveAs (marker m)
        )