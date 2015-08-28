module UI.Term (
    display,
    initializeDisplay,
    render,
    mainLoop,
) where

import Control.Monad (liftM)
import Control.Monad.State (StateT, runStateT, get, lift)
import qualified UI.NCurses as Curses

import Interface.Board (BoardState(..), Cell)


data DisplayState = DisplayState {
        win             :: Curses.Window
    }
type AlterDisplay a = StateT DisplayState Curses.Curses a

display :: Curses.Curses a -> IO a
display = Curses.runCurses


initializeDisplay :: BoardState b => b -> Curses.Curses DisplayState
initializeDisplay b = Curses.setEcho False >> liftM DisplayState Curses.defaultWindow


render :: AlterDisplay ()
render = do
        dispState <- get
        lift $ Curses.updateWindow (win dispState) update
        lift $ Curses.render
    where
    update = do
        Curses.moveCursor 1 10
        Curses.drawString "Hello world!"
        Curses.moveCursor 3 10
        Curses.drawString "(press q to quit)"
        Curses.moveCursor 0 0


mainLoop :: AlterDisplay ()
mainLoop = get >>= (\dispState -> lift $ waitFor (win dispState))

waitFor :: Curses.Window -> Curses.Curses ()
waitFor w = Curses.getEvent w Nothing >>= handleEvent
    where
    oneOfChars e chrs = e `elem` map Curses.EventCharacter chrs
    handleEvent Nothing = waitFor w
    handleEvent (Just ev)
        | ev `oneOfChars` ['q', 'Q'] = return ()
        | otherwise = waitFor w
