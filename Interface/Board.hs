module Interface.Board where

type Cell = (Int, Int)

class BoardState b where
    getLiving :: b -> [Cell]
    isLiving :: Cell -> b -> Bool
    alter :: Cell -> b -> b
    next :: b -> b
    previous :: b -> b


translate :: Cell -> Cell -> Cell
translate (x, y) (xVect, yVect) = (x + xVect, y + yVect)
