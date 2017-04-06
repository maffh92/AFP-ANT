{-|

Module: Ant
Description: Ant monad

-}
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
  -- , while
  -- , if'
  -- ** Marker
  , Marker
  , zero
  , one
  , two
  , three
  , four
  , five
  -- ** Turning direction
  , LeftOrRight
  , left
  , right
  -- ** Code generation
  , compile
  , compileProg
  , compileToFile
  ) where

import           Ant.Base
import           Ant.Monad

import           Control.Lens
import           Data.Map     (toList)

-- | Compile ant code to the form that the simulator uses, and return it as a string.
compile :: AntM L () -> String
compile = showCmds . toList . view commands . compileProg

-- | Compile ant code and write it to a file.
compileToFile :: FilePath -> AntM L () -> IO ()
compileToFile path = writeFile path . compile

-- | Compile ant code.
compileProg :: AntM L () -> Program L
compileProg = fst . snd . runAntM z

-- | The condition when the ant sees a friend
friend :: Condition
friend = Friend

-- | The condition when the ant sees a foe
foe :: Condition
foe = Foe

-- | The condition when the ant sees a friend carrying food
friendWithFood :: Condition
friendWithFood = FriendWithFood

-- | The condition when the ant sees a foe carrying food
foeWithFood :: Condition
foeWithFood = FoeWithFood

-- | The condition when the ant sees food
food :: Condition
food = Food

-- | The condition when the ant sees a rock
rock :: Condition
rock = Rock

-- | The condition when the ant sees one of its own team's markers
marker :: Marker -> Condition
marker = Marker

-- | The condition when the ant sees an enemy marker
foeMarker :: Condition
foeMarker = FoeMarker

-- | The condition when the ant sees its home base
home :: Condition
home = Home

-- | The condition when the ant sees the enemy's home base
foeHome :: Condition
foeHome = FoeHome

-- | Sense at the ant's current position
here :: SenseDir
here = Here

-- | Sense directly ahead of the ant's current position
ahead :: SenseDir
ahead = Ahead

-- | Sense left ahead of the ant's current position
leftAhead :: SenseDir
leftAhead = LeftAhead

-- | Sense right ahead of the ant's current position
rightAhead :: SenseDir
rightAhead = RightAhead

-- | Marker 0
zero :: Marker
zero = Zero

-- | Marker 1
one :: Marker
one = One

-- | Marker 2
two :: Marker
two = Two

-- | Marker 3
three :: Marker
three = Three

-- | Marker 4
four :: Marker
four = Four

-- | Marker 5
five :: Marker
five = Five

-- | Left
left :: LeftOrRight
left = IsLeft

-- | Right
right :: LeftOrRight
right = IsRight
