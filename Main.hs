module Main where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.Builder (builderNew, builderAddFromFile, builderGetObject)
import qualified Graphics.Rendering.Cairo as Cairo

import Data.Board (Board, newBoard, emptyBoard)
import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), initDisplay, Mem, newMem, atomically, board,
                    display, alterBoard, alterDisplay)


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


drawBoard :: (BoardState b, GTK.DrawableClass v) => v -> Mem b -> IO ()
drawBoard view mem = selectLivingCells mem
                    >>= \cs -> GTK.renderWithDrawable view (drawBoardAction cs)


drawBoardAction :: [Cell] -> Cairo.Render ()
drawBoardAction visible =  clearBoard >> sequence_ (map drawCell visible)


selectLivingCells :: BoardState b => Mem b -> IO [Cell]
selectLivingCells mem = atomically $ do
            disp <- display mem
            living <- liftM getLiving $ board mem
            return $ inView disp living
    where
    inView disp = let (firstx, firsty) = firstCell disp
                      (lastx, lasty) = lastCell disp in filter $
                \(x, y) -> firstx < x && x < lastx && firsty < y && y < lasty

clearBoard :: Cairo.Render()
clearBoard = Cairo.setSourceRGB 1 1 1 >> Cairo.paint


drawCell :: Cell -> Cairo.Render ()
drawCell (col, row) = let x = 10 * fromIntegral col
                          y = 10 * fromIntegral row in do
        Cairo.setSourceRGB 0 0 0
        Cairo.setLineWidth 2

        Cairo.moveTo x y
        Cairo.lineTo (x + 10) y
        Cairo.lineTo (x + 10) (y + 10)
        Cairo.lineTo x (y + 10)
        Cairo.closePath
        Cairo.strokePreserve

        Cairo.setSourceRGB 0.67 0.67 0.67
        Cairo.fill

        Cairo.stroke
