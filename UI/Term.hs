module UI.Term (
    display,
    initializeDisplay,
    render,
    mainLoop,
) where

import Control.Monad (liftM)
import qualified UI.NCurses as Curses

import Interface.Board (BoardState(..), Cell)


data DisplayState = DisplayState {
        win             :: Curses.Window
    }

display :: Curses.Curses a -> IO a
display = Curses.runCurses


initializeDisplay :: BoardState b => b -> Curses.Curses DisplayState
initializeDisplay b = Curses.setEcho False >> liftM DisplayState Curses.defaultWindow


render :: DisplayState -> Curses.Curses DisplayState
render dispState = Curses.updateWindow (win dispState) update >> Curses.render >> return dispState
    where
    update = do
        Curses.moveCursor 1 10
        Curses.drawString "Hello world!"
        Curses.moveCursor 3 10
        Curses.drawString "(press q to quit)"
        Curses.moveCursor 0 0


mainLoop :: DisplayState -> Curses.Curses DisplayState
mainLoop dispState = waitFor (win dispState) >> return dispState

waitFor :: Curses.Window -> Curses.Curses ()
waitFor w = Curses.getEvent w Nothing >>= handleEvent
    where
    oneOf e chrs = e `elem` map Curses.EventCharacter chrs
    handleEvent Nothing = waitFor w
    handleEvent (Just ev)
        | ev `oneOf` ['q', 'Q'] = return ()
        | otherwise = waitFor w
