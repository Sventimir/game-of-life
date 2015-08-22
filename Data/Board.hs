module Data.Board (
    Board(..)
) where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Interface.Board (BoardState(..), Cell)

data Board = Board {
                current         :: Set Cell,
                previous        :: Maybe Board
            } deriving Show

instance BoardState Board where
    _isLiving cell (Board s _) = cell `Set.member` s
    _alter cell brd@(Board s _)
        | _isLiving cell brd = Board (cell `Set.delete` s) (Just brd)
        | otherwise = Board (cell `Set.insert` s) (Just brd)
    _previous (Board _ p) = fromMaybe (Board Set.empty Nothing) p
    _next brd@(Board s _) = let s' = s `Set.union` born Set.\\ dying
                                born = Set.filter isBorn allNeighbours
                                dying = Set.filter isDying s
                                allNeighbours = Set.unions $ map neighbours $ Set.toList s
                            in Board s' (Just brd)
        where
        isDying c = neighbourCount c < 2 || neighbourCount c > 3
        isBorn c = neighbourCount c == 3
        neighbourCount c = Set.size $ neighbours c `Set.intersection` s


neighbours :: Cell -> Set Cell
neighbours (x, y) = Set.fromList $ [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
                                   (x - 1, y), (x + 1, y),
                                   (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
