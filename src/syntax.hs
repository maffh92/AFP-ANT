{-#LANGUAGE GADTs, FlexibleInstances #-}

import qualified Data.Map as M

data Direction = AHead | Left | Right deriving Show
data Cond = Food | Home deriving Show
type Env = M.Map Int Expr
-- type State = M.Map Int Expr
type StateId = Int
type Cell = Int

data Expr where
    Sense  :: Direction -> StateId -> StateId -> Cond -> Expr
    Mark   :: Cell -> StateId -> Expr
    Unmark :: Cell -> StateId -> Expr
    PickUp :: StateId -> Expr
    Drop   :: StateId -> Expr
    Turn   :: Direction -> StateId -> Expr
    Move   :: StateId -> StateId -> Expr
    Flip   :: Int -> StateId -> StateId -> Expr
 deriving Show

newtype State s a = State {runState :: s -> (s,a)}

instance Functor (State s) where
    fmap = undefined 

instance Applicative (State s) where
    pure = undefined
    (<*>) = undefined 

instance Monad (State s) where
    return a = State $ \s -> (s,a)
    (State state) >>= f = State $ \s -> 
                            let (s1,a) = state s
                                (State state2) = f a
                            in state2 s1


sense :: Direction -> StateId -> StateId -> Cond -> State Env Int
sense dir st1 st2 cond = put (Sense dir st1 st2 cond)

mark   :: Cell -> StateId -> State Env Int
mark c st1 = put (Mark c st1)

unmark :: Cell -> StateId -> State Env Int
unmark c st1 = put (Unmark c st1)

pickUp :: StateId -> State Env Int
pickUp st1 = put (PickUp st1)

dropF   :: StateId -> State Env Int
dropF st1 = put (Drop st1)

turn   :: Direction -> StateId -> State Env Int
turn d st1 = put (Turn d st1)

move   :: StateId -> StateId -> State Env Int
move st1 st2 = put (Move st1 st2)

flip   :: Int -> StateId -> StateId -> State Env Int
flip n st1 st2 = put (Flip n st1 st2)


put :: Expr -> State Env Int
put expr = State $ \env -> 
                let stateId = getState env
                in (M.insert stateId expr env, stateId)


--Stategy
findFood :: State Env Int -> State Env Int
findFood state = do
    initialState <- state
    foodAhead <- pickUp initialState
    b <- sense AHead foodAhead initialState Food
    return b


randomStrategy :: State Env Int -> State Env Int
randomStrategy state = do
    init <- state
    food <- findFood state
    food2 <- findFood $ return food
    dropFood <- dropF food2
    return dropFood


getState :: Env -> Int
getState states | M.size states > 0 = (fst $ M.findMax states) + 1
                | otherwise         = 0


-- do 
--  senseFood
--  