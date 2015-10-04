module UI.SharedMem (
    Display(..),
    initDisplay,
    Mem,
    atomically,
    newMem,
    board,
    display,
    alterBoard,
    alterDisplay,
    cellCoords,
    cellAt,
    mapPair
) where

import Control.Monad (liftM)
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, modifyTVar, atomically)

import Interface.Board (Cell)


data Display = Display {
        firstCell           :: Cell,
        lastCell            :: Cell,
        autoNext            :: Bool
    }

initDisplay :: Display
initDisplay = Display {
        firstCell = (0, 0),
        lastCell = (0, 0),
        autoNext = False
    }


data Mem b = Mem (TVar b) (TVar Display)


newMem :: b -> Display -> STM (Mem b)
newMem brd displ = do
        b <- newTVar brd
        d <- newTVar displ
        return $ Mem b d

board :: Mem b -> STM b
board (Mem b _) = readTVar b

display :: Mem b -> STM Display
display (Mem _ d) = readTVar d

alterBoard :: (b -> b) -> Mem b -> STM ()
alterBoard f (Mem b d) = modifyTVar b f

alterDisplay :: (Display -> Display) -> Mem b -> STM ()
alterDisplay f (Mem b d) = modifyTVar d f


cellCoords :: Mem b -> Cell -> STM (Int, Int)
cellCoords mem cell = liftM (cellToCoords . translate cell . firstCell) (display mem)
    where
    cellToCoords = mapPair (* 10)

cellAt :: Mem b -> (Int, Int) -> STM Cell
cellAt mem coords = liftM (translate (toCell coords) . firstCell) (display mem)
    where
    toCell = mapPair $ flip div 10

mapPair :: Num n => (n -> n) -> (n, n) -> (n, n)
mapPair f (a, b) = (f a, f b)

translate :: Cell -> Cell -> Cell
translate (x, y) (xv, yv) = (x + xv, y + yv)
