module Interface.Board where

import Control.Monad.State (State, state, modify)


type Cell = (Int, Int)

class BoardState b where
    _isLiving :: Cell -> b -> Bool
    _alter :: Cell -> b -> b
    _next :: b -> b
    _previous :: b -> b


isLiving :: BoardState b => Cell -> State b Bool
isLiving cell = state $ \b -> (_isLiving cell b, b)

alter :: BoardState b => Cell -> State b ()
alter cell = modify $ _alter cell

next :: BoardState b => State b ()
next = modify _next

previous :: BoardState b => State b ()
previous = modify _previous
