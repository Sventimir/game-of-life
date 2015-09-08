module Main where

import Control.Monad.State (runStateT, lift)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (Cell, next)
import Data.Board (Board(..))


main :: IO ()
main = putStrLn "Hello World!"
