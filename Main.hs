module Main where

import Control.Monad.State (runState)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (next)
import Data.Board (Board(..))


main :: IO ()
main = do
    putStrLn "Hello Life!"
    print $ current initial
    print . current . snd $ runState next initial
    print . current . snd $ runState (next >> next) initial

initial :: Board
initial = Board (Set.fromList init) Nothing
    where
    init = zip [(-1)..1] $ repeat 0
