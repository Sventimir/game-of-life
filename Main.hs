module Main where

import Control.Monad.State (runStateT, lift)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (Cell, next)
import Data.Board (Board(..))
import UI.Term (display, initializeDisplay, initializeUI, render, mainLoop)


main :: IO ()
main = display $ initializeUI initial >>=
            runStateT (render initializeDisplay >> mainLoop) >> return ()
    where
    initial = Board Set.empty Nothing
