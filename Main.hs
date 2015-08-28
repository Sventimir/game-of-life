module Main where

import Control.Monad.State (StateT, runStateT, liftIO, get)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (Cell, next)
import Data.Board (Board(..))
import UI.Term (display, initializeDisplay, render, mainLoop)


main :: IO ()
main = display $ initializeDisplay initial >>= render >>= mainLoop >> return ()
    where
    initial = Board Set.empty Nothing
