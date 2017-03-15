{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module AntState where

{- describes the output state machine -}
data TurnDir   = Left | Right deriving (Show, Eq)
data SenseDir  = Here | Ahead | LeftAhead | RightAhead deriving (Show, Eq)
data Marker    = Zero | One | Two | Three | Four | Five deriving Eq

data Condition = Friend
               | Foe
               | FriendWithFood
               | FoeWithFood
               | Food
               | Rock
               | Marker Marker
               | FoeMarker
               | Home
               | FoeHome
               deriving (Show, Eq)

type State = Int
data Command a = Sense SenseDir a a Condition
               | Mark Marker a
               | Unmark Marker a
               | PickUp a a
               | Drop a
               | Turn TurnDir a
               | Move a a
               | Flip Int a a
               deriving (Show, Eq, Functor, Foldable, Traversable)

instance Show Marker where
    show Zero   = "0"
    show One    = "1"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"


showCmds :: Show a => [(a, Command a)] -> String
showCmds = unlines . map showInstr
  where
    showInstr (a, cmd) = concat [show cmd, " ; state ", show a, ":"]

