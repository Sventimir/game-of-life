module Main where

import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.Builder (builderNew, builderAddFromFile, builderGetObject)

import Data.Board (Board, newBoard, emptyBoard)
import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), initDisplay, Mem, newMem, atomically,
                    display, alterBoard, alterDisplay)
import UI.Board (drawBoard, highlightCell)


main :: IO ()
main = do
        state <- atomically $ newMem initBoard initDisplay
        GTK.initGUI

        builder <- builderNew
        builderAddFromFile builder "UI/life.glade"

        mainWin <- builderGetObject builder GTK.castToWindow "mainWin"
        mainWin `GTK.on` GTK.deleteEvent $ liftIO GTK.mainQuit >> return False

        GTK.widgetShowAll mainWin

        gameView <- builderGetObject builder GTK.castToDrawingArea "gameView"
        viewport <- GTK.widgetGetDrawWindow gameView
        [nextBtn, prevBtn] <- sequence $
                    map (getButton builder) ["nextBtn", "prevBtn"]
        alterBtn <- builderGetObject builder GTK.castToToggleButton "alterBtn"

        arrowCursor <- GTK.cursorNew GTK.Arrow
        GTK.drawWindowSetCursor viewport $ Just arrowCursor

        GTK.widgetAddEvents gameView [GTK.PointerMotionMask]

        gameView `GTK.on` GTK.exposeEvent $ do
            liftIO $ do
                updateDisplaySize gameView state
                drawBoard viewport state
            return False

        gameView `GTK.on` GTK.motionNotifyEvent $ do
            liftIO $ GTK.toggleButtonGetActive alterBtn >>= \alt -> when alt $
                    liftM cellFromCoordinates (GTK.widgetGetPointer gameView)
                    >>= highlightCell viewport state
            return False

        gameView `GTK.on` GTK.leaveNotifyEvent $ do
            liftIO $ drawBoard viewport state
            return False

        gameView `GTK.on` GTK.buttonPressEvent $ do
            liftIO $ GTK.toggleButtonGetActive alterBtn >>= \alt -> when alt $ do
                cell <- liftM cellFromCoordinates (GTK.widgetGetPointer gameView)
                atomically $ alterBoard (alter cell) state
            return False

        nextBtn `GTK.on` GTK.buttonReleaseEvent $ do
            liftIO $ do
                atomically $ alterBoard next state
                drawBoard viewport state
            return False

        prevBtn `GTK.on` GTK.buttonReleaseEvent $ do
            liftIO $ do
                atomically $ alterBoard previous state
                drawBoard viewport state
            return False

        GTK.mainGUI
    where
    initBoard = newBoard [(3, 4), (3, 5), (3, 6)]
    getButton bld name = builderGetObject bld GTK.castToButton name


updateDisplaySize :: (BoardState b, GTK.WidgetClass v) => v -> Mem b -> IO ()
updateDisplaySize view mem = liftIO $ do
        (width, height) <- liftM rectSize $ GTK.widgetGetAllocation view
        let size = (width `div` 10, height `div` 10)
        atomically $ do
            first <- liftM firstCell $ display mem
            alterDisplay (update $ translate first size) mem
    where
    rectSize (GTK.Rectangle fx fy lx ly) = (lx - fx, ly - fy)
    update cell disp = disp { lastCell = cell }

cellFromCoordinates :: (Int, Int) -> Cell
cellFromCoordinates (x, y) = (x `div` 10, y `div` 10)
