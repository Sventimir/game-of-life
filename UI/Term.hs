module UI.Term (
    display,
    initializeUI,
    initializeDisplay,
    render,
    mainLoop,
) where

import Control.Monad (liftM)
import Control.Monad.State (StateT, runStateT, get, lift)
import Data.Text (pack)
import qualified UI.NCurses as Curses

import Interface.Board (BoardState(..), Cell)


data DisplayState = DisplayState {
        win             :: Curses.Window,
        screenSize      :: (Integer, Integer)
    }
type AlterDisplay a = StateT DisplayState Curses.Curses a

display :: Curses.Curses a -> IO a
display = Curses.runCurses


initializeUI :: BoardState b => b -> Curses.Curses DisplayState
initializeUI b = do
        Curses.setEcho False
        window <- Curses.defaultWindow
        screen <- Curses.screenSize
        return DisplayState {
            win = window,
            screenSize = screen
        }


render :: (DisplayState -> Curses.Update a) -> AlterDisplay a
render update = do
        dispState <- get
        result <- lift $ Curses.updateWindow (win dispState) (update dispState)
        lift $ Curses.render
        return result

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


initializeDisplay :: DisplayState -> Curses.Update ()
initializeDisplay st = do
        write AlCenter 1 "Welcome to the Game of Life" st
        write AlCenter 2 "(press h for help or q to quit)" st
        Curses.moveCursor 0 0


data Alignment = AlLeft | AlCenter | AlRight deriving (Show, Eq)
write :: Alignment -> Integer -> String -> DisplayState -> Curses.Update Integer
write al row txt st = let width = snd $ screenSize st
                          space = width - (fromIntegral $ length txt) in do
        Curses.moveCursor row $ getCol al (max 0 space)
        Curses.drawText $ pack txt
        return space
    where
    getCol AlLeft = const 0
    getCol AlCenter = (`div` 2)
    getCol AlRight = id
