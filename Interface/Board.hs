module Interface.Board where

import Control.Monad.State (StateT, get, state, modify)


type Cell = (Int, Int)

class BoardState b where
    _isLiving :: Cell -> b -> Bool
    _alter :: Cell -> b -> b
    _next :: b -> b
    _previous :: b -> b


isLiving :: (BoardState b, Monad m) => Cell -> StateT b m Bool
isLiving cell = state $ \b -> (_isLiving cell b, b)

alter :: (BoardState b, Monad m) => Cell -> StateT b m ()
alter cell = modify $ _alter cell

next :: (BoardState b, Monad m) => StateT b m b
next = get >>= \s -> modify _next >> return s

previous :: (BoardState b, Monad m) => StateT b m b
previous = get >>= \s -> modify _previous >> return s
