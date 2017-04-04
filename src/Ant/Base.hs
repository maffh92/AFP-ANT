{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Ant.Base where

import           GHC.Generics

{- describes the output state machine -}
data LeftOrRight
  = IsLeft
  | IsRight
  deriving (Ord, Eq, Generic)

instance Show LeftOrRight where
  show Ant.Base.IsLeft  = "Left"
  show Ant.Base.IsRight = "Right"

data SenseDir
  = Here
  | Ahead
  | LeftAhead
  | RightAhead
  deriving (Ord, Eq, Generic, Show)

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
  deriving (Eq, Ord, Generic, Show)


data Command a
  = Sense SenseDir a a Condition
  | Mark Marker a
  | Unmark Marker a
  | PickUp a a
  | Drop a
  | Turn LeftOrRight a
  | Move a a
  | Flip Int a a
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Show a => Show (Command a) where
  show (Sense q w e r) = "Sense "  ++ concat [show q, " ", show w, " ", show e, " ", show r]
  show (Mark q w)      = "Mark "   ++ concat [show q, " ", show w]
  show (Unmark q w)    = "Unmark " ++ concat [show q, " ", show w]
  show (PickUp q w)    = "PickUp " ++ concat [show q, " ", show w]
  show (Drop q)        = "Drop "   ++ concat [show q]
  show (Turn q w)      = "Turn "   ++ concat [show q, " ", show w]
  show (Move q w)      = "Move "   ++ concat [show q, " ", show w]
  show (Flip q w e)    = "Flip "   ++ concat [show q, " ", show w, " ", show e]

showCmds :: Show a => [(a, Command a)] -> String
showCmds = unlines . map showInstr
  where
    showInstr (a, cmd) = concat [show cmd, " ; state ", show a, ":"]

