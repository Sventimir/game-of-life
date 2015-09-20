module Main where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.Builder (builderNew, builderAddFromFile, builderGetObject)

import Data.Board (Board, newBoard, emptyBoard)
import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), initDisplay, Mem, newMem, atomically,
                    display, alterBoard, alterDisplay)
import UI.Board (drawBoard)


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

        gameView `GTK.on` GTK.exposeEvent $ do
            liftIO $ do
                updateDisplaySize gameView state
                drawBoard viewport state
            return False

        nextBtn <- builderGetObject builder GTK.castToButton "nextBtn"
        nextBtn `GTK.on` GTK.buttonReleaseEvent $ do
            liftIO $ do
                atomically $ alterBoard next state
                drawBoard viewport state
            return False

        GTK.mainGUI
    where
    initBoard = newBoard [(3, 4), (3, 5), (3, 6)]


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
