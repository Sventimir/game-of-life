module UI.Board (
    clearBoard,
    drawBoard,
    highlightCell
) where

import Control.Monad (liftM)

import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo

import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), Mem, atomically, board, display, alterDisplay)


type Color = (Double, Double, Double) -- RGB


clearBoard :: (GTK.DrawableClass v) => v -> IO ()
clearBoard view = GTK.renderWithDrawable view clearBoardAction


clearBoardAction :: Cairo.Render()
clearBoardAction = Cairo.setSourceRGB 1 1 1 >> Cairo.paint


drawBoard :: (BoardState b, GTK.DrawableClass v) => v -> Mem b -> IO ()
drawBoard view mem = selectLivingCells mem
                    >>= \cs -> GTK.renderWithDrawable view (drawBoardAction cs)


drawBoardAction :: [Cell] -> Cairo.Render ()
drawBoardAction visible = do
        clearBoardAction
        sequence_ $ map (drawCell (0.67, 0.67, 0.67)) visible


selectLivingCells :: BoardState b => Mem b -> IO [Cell]
selectLivingCells mem = atomically $ do
            disp <- display mem
            living <- liftM getLiving $ board mem
            return $ inView disp living
    where
    inView disp living = let (firstx, firsty) = firstCell disp
                             (lastx, lasty) = lastCell disp
                         in [(x - firstx, y - firsty) | (x, y) <- living,
                            firstx < x && x < lastx && firsty < y && y < lasty]


highlightCell :: (BoardState b, GTK.DrawableClass v) => v -> Mem b -> Cell -> IO ()
highlightCell view mem cell = selectLivingCells mem
            >>= \liv -> GTK.renderWithDrawable view
                    (drawBoardAction liv >> drawCell (0.8, 0.8, 0.8) cell)


drawCell :: Color -> Cell -> Cairo.Render ()
drawCell (r, g, b) cell = do
        Cairo.setSourceRGB 0 0 0
        Cairo.setLineWidth 2
        squarePath cell
        Cairo.strokePreserve
        Cairo.setSourceRGB r g b
        Cairo.fill
        Cairo.stroke

eraseCell :: Cell -> Cairo.Render ()
eraseCell cell = do
        Cairo.setSourceRGB 1 1 1
        Cairo.setLineWidth 2
        squarePath cell
        Cairo.strokePreserve
        Cairo.fill
        Cairo.stroke


squarePath :: Cell -> Cairo.Render ()
squarePath (col, row) = let x = 10 * fromIntegral col
                            y = 10 * fromIntegral row in do
        Cairo.moveTo x y
        Cairo.lineTo (x + 9) y
        Cairo.lineTo (x + 9) (y + 9)
        Cairo.lineTo x (y + 9)
        Cairo.closePath
