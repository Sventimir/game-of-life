module UI.SharedMem (
    Display(..),
    initDisplay,
    Mem,
    atomically,
    newMem,
    board,
    display,
    alterBoard,
    alterDisplay
) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, modifyTVar, atomically)

import Interface.Board (Cell)


data Display = Display {
        firstCell           :: Cell,
        lastCell            :: Cell
    }

initDisplay :: Display
initDisplay = Display {
        firstCell = (0, 0),
        lastCell = (0, 0)
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
