module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.Builder (builderNew, builderAddFromFile, builderGetObject)

import Data.Board (Board, newBoard, emptyBoard)
import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), initDisplay, Mem, newMem, atomically,
                    display, alterBoard, alterDisplay, cellAt, mapPair)
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
        [alterBtn, playBtn] <- sequence $
                    map (getToggle builder) ["alterBtn", "playBtn"]

        arrowCursor <- GTK.cursorNew GTK.Arrow
        GTK.drawWindowSetCursor viewport $ Just arrowCursor

        GTK.widgetAddEvents gameView [GTK.PointerMotionMask]

        mainWin `GTK.on` GTK.keyPressEvent $ keyMoveDisplay state viewport

        gameView `GTK.on` GTK.exposeEvent $ do
            liftIO $ do
                updateDisplaySize gameView state
                drawBoard viewport state
            return False

        gameView `GTK.on` GTK.motionNotifyEvent $ do
            liftIO $ GTK.toggleButtonGetActive alterBtn >>= \alt -> when alt $
                    liftM (mapPair $ flip div 10) (GTK.widgetGetPointer gameView)
                    >>= highlightCell viewport state
            return False

        gameView `GTK.on` GTK.leaveNotifyEvent $ do
            liftIO $ drawBoard viewport state
            return False

        gameView `GTK.on` GTK.buttonPressEvent $ do
            liftIO $ GTK.toggleButtonGetActive alterBtn >>= \alt -> when alt $ do
                cell <- GTK.widgetGetPointer gameView >>= cellFromCoordinates state
                atomically $ alterBoard (alter cell) state
            return False

        let withBoard f = liftIO (alterBoardAction state viewport f) >> return False
        nextBtn `GTK.on` GTK.buttonReleaseEvent $ withBoard next
        prevBtn `GTK.on` GTK.buttonReleaseEvent $ withBoard previous
        playBtn `GTK.on` GTK.buttonReleaseEvent $ do
            isOn <- liftIO $ GTK.toggleButtonGetActive playBtn
            liftIO . atomically $ alterDisplay (\d -> d { autoNext = not isOn }) state
            return False

        forkIO $ autoPlay state viewport

        GTK.mainGUI
    where
    initBoard = newBoard [(3, 4), (3, 5), (3, 6)]
    getButton bld name = builderGetObject bld GTK.castToButton name
    getToggle bld name = builderGetObject bld GTK.castToToggleButton name


alterBoardAction :: BoardState b => Mem b -> GTK.DrawWindow -> (b -> b) -> IO ()
alterBoardAction state viewport f = do
        atomically $ alterBoard f state
        drawBoard viewport state


keyMoveDisplay :: BoardState b => Mem b -> GTK.DrawWindow -> GTK.EventM GTK.EKey Bool
keyMoveDisplay state viewport = do
        movement <- liftM keyToMvmnt GTK.eventKeyVal
        liftIO $ do
            atomically $ alterDisplay (move movement) state
            drawBoard viewport state
        return True
    where
    keyToMvmnt 65361 = ((-1), 0)        -- left
    keyToMvmnt 65362 = (0, (-1))        -- up
    keyToMvmnt 65363 = (1, 0)           -- right
    keyToMvmnt 65364 = (0, 1)           -- down
    keyToMvmnt _ = (0, 0)
    move (x, y) displ = let (fx, fy) = firstCell displ
                            (lx, ly) = lastCell displ in displ {
                                    firstCell = (fx + x, fy + y),
                                    lastCell = (lx + x, ly + y)
                                }


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

cellFromCoordinates :: Mem b -> (Int, Int) -> IO Cell
cellFromCoordinates state coords = atomically $ cellAt state coords


autoPlay :: BoardState b => Mem b -> GTK.DrawWindow -> IO ()
autoPlay state view = do
        auto <- atomically $ liftM autoNext (display state)
        when auto $ GTK.postGUIAsync $ alterBoardAction state view next
        threadDelay 1000000
        autoPlay state view
