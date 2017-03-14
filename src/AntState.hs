{-#LANGUAGE DeriveFunctor,DeriveFoldable #-}
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
--St is used as a State number.
type Command = Command' Int  
data Command' st  = Sense SenseDir st st Condition
               | Mark Marker st
               | Unmark Marker st
               | PickUp st st
               | Drop st
               | Turn TurnDir st
               | Move st st
               | Flip Int st st
               deriving (Show, Eq,Functor,Foldable)


instance Show Marker where
    show Zero   = "0"
    show One    = "1"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"


compile :: [Command] -> String
compile = unlines . map show