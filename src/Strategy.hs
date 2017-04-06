{-# LANGUAGE RecursiveDo                #-}


module Strategy where

import           Abstractions
import           Ant
import           Control.Monad.Fix


strategy :: (MonadFix m, Label l) => AntT m l ()
strategy =
  frequency [ (8, forever searchForFood )
            , (2, highWayMakers)
            , (2, forever $ search Nothing foeHome >> searchForFood)
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
      move (turnAll >> move_ (turnAll >> turn d2 >> goto l) >> turnAll >> turn d2)
           (turn d2)


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

guardHome :: (MonadFix m, Label l) => AntT m l ()
guardHome = do
  search Nothing rock
  search Nothing home
  forever $ do
    mark one
    choose [twice left, twice right]
    search Nothing (marker one)
      where twice d = turn d >> turn d

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


clearTrail :: (MonadFix m, Label l) => Marker -> AntT m l ()
clearTrail m  = do
  while ((ahead :=: (marker m)) :|: (leftAhead :=: (marker m))  :|: (rightAhead :=: (marker m))) 
        ( do
          unmark m
          moveAs (marker m)
        )