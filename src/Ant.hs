module Ant
  ( module Ant.Monad
  -- ** Condition
  , Condition
  , friend
  , foe
  , friendWithFood
  , foeWithFood
  , food
  , rock
  , marker
  , foeMarker
  , home
  , foeHome
  -- ** Sensor direction
  , SenseDir
  , here
  , ahead
  , leftAhead
  , rightAhead
  -- ** Marker
  , Marker
  , zero
  , one
  , two
  , three
  , four
  , five
  -- ** Turning direction
  , TurnDir
  , left
  , right
  -- ** Code generation
  , compile
  ) where

import           Ant.Base as B
import           Ant.Monad

import           Control.Lens
import           Data.Map     (toList)

compile :: AntM L () -> String
compile = showCmds . toList . view commands . fst . snd . runAntM z

friend :: Condition
friend = Friend

foe :: Condition
foe = Foe

friendWithFood :: Condition
friendWithFood = FriendWithFood

foeWithFood :: Condition
foeWithFood = FoeWithFood

food :: Condition
food = Food

rock :: Condition
rock = Rock

marker :: Marker -> Condition
marker = Marker

foeMarker :: Condition
foeMarker = FoeMarker

home :: Condition
home = Home

foeHome :: Condition
foeHome = FoeHome

here :: SenseDir
here = Here

ahead :: SenseDir
ahead = Ahead

leftAhead :: SenseDir
leftAhead = LeftAhead

rightAhead :: SenseDir
rightAhead = RightAhead

zero :: Marker
zero = Zero

one :: Marker
one = One

two :: Marker
two = Two

three :: Marker
three = Three

four :: Marker
four = Four

five :: Marker
five = Five

left :: TurnDir
left = B.Left

right :: TurnDir
right = B.Right
