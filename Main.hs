module Main where

import Control.Monad.State (StateT, runStateT, liftIO, get)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (Cell, next)
import Data.Board (Board(..))


main :: IO (Board, Board)
main = do
    putStrLn "Hello Life!"
    runStateT (loop 10 showNext) initial

initial :: Board
initial = Board (Set.fromList init) Nothing
    where
    init = zip [(-1)..1] $ repeat 0


loop :: Int -> StateT Board IO Board -> StateT Board IO Board
loop i s
    | i > 0 = s >> loop (pred i) s
    | otherwise = s >> get >>= return

showNext :: StateT Board IO Board
showNext = next >>= \b -> liftIO (print b >> return b)

