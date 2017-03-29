{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Ant.Base where

import           GHC.Generics

{- describes the output state machine -}
data TurnDir
  = Left
  | Right
  deriving (Show, Ord, Eq, Generic)

data SenseDir
  = Here
  | Ahead
  | LeftAhead
  | RightAhead
  deriving (Show, Ord, Eq, Generic)

data Marker
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Generic)

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
  deriving (Eq, Ord, Generic)

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

data Command a
  = Sense SenseDir a a Condition
  | Mark Marker a
  | Unmark Marker a
  | PickUp a a
  | Drop a
  | Turn TurnDir a
  | Move a a
  | Flip Int a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

showCmds :: Show a => [(a, Command a)] -> String
showCmds = unlines . map showInstr
  where
    showInstr (a, cmd) = concat [show cmd, " ; state ", show a, ":"]

