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
data Command   = Sense SenseDir Int Int Condition
               | Mark Marker State
               | Unmark Marker State
               | PickUp State State
               | Drop State
               | Turn TurnDir State
               | Move State State
               | Flip Int State State
               deriving (Show, Eq)

instance Show Marker where
    show Zero   = "0"
    show One    = "1"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"


compile :: [Command] -> String
compile = unlines . map show

