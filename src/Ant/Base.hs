{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Ant.Base where

{- describes the output state machine -}
data TurnDir
  = DLeft
  | DRight
  deriving Eq

instance Show TurnDir where
  show DLeft  = "Left"
  show DRight = "Right"

data SenseDir
  = Here
  | Ahead
  | LeftAhead
  | RightAhead
  deriving (Show, Eq)

data Marker
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  deriving Eq

instance Enum Marker where
  fromEnum x = case x of
    { Zero -> 0; One -> 1; Two -> 2; Three -> 3; Four -> 4; Five -> 5 }

  toEnum x = case x `mod` 6 of
    { 0 -> Zero; 1 -> One; 2 -> Two; 3 -> Three; 4 -> Four; 5 -> Five }

instance Ord Marker where
  x `compare` y = fromEnum x `compare` fromEnum y

instance Show Marker where
  show = show . fromEnum

data Condition
  = Friend
  | Foe
  | FriendWithFood
  | FoeWithFood
  | Food
  | Rock
  | Marker Marker
  | FoeMarker
  | Home
  | FoeHome
  deriving Eq

instance Show Condition where
  show Friend         = "Friend"
  show Foe            = "Foe"
  show FriendWithFood = "FriendWithFood"
  show FoeWithFood    = "FoeWithFood"
  show Food           = "Food"
  show Rock           = "Rock"
  show (Marker l)     = show l
  show FoeMarker      = "FoeMarker"
  show Home           = "Home"
  show FoeHome        = "FoeHome"

type State = Int

data Command a
  = Sense SenseDir a a Condition
  | Mark Marker a
  | Unmark Marker a
  | PickUp a a
  | Drop a
  | Turn TurnDir a
  | Move a a
  | Flip Int a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

showCmds :: Show a => [(a, Command a)] -> String
showCmds = unlines . map showInstr
  where
    showInstr (a, cmd) = concat [show cmd, " ; state ", show a, ":"]

