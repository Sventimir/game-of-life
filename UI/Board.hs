module UI.Board (
    clearBoard,
    drawBoard
) where

import Control.Monad (liftM)

import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo

import Interface.Board (Cell, BoardState(..), translate)
import UI.SharedMem (Display(..), Mem, atomically, board, display)


clearBoard :: (GTK.DrawableClass v) => v -> IO ()
clearBoard view = GTK.renderWithDrawable view clearBoardAction


clearBoardAction :: Cairo.Render()
clearBoardAction = Cairo.setSourceRGB 1 1 1 >> Cairo.paint


drawBoard :: (BoardState b, GTK.DrawableClass v) => v -> Mem b -> IO ()
drawBoard view mem = selectLivingCells mem
                    >>= \cs -> GTK.renderWithDrawable view (drawBoardAction cs)


drawBoardAction :: [Cell] -> Cairo.Render ()
drawBoardAction visible =  clearBoardAction >> sequence_ (map drawCell visible)


selectLivingCells :: BoardState b => Mem b -> IO [Cell]
selectLivingCells mem = atomically $ do
            disp <- display mem
            living <- liftM getLiving $ board mem
            return $ inView disp living
    where
    inView disp = let (firstx, firsty) = firstCell disp
                      (lastx, lasty) = lastCell disp in filter $
                \(x, y) -> firstx < x && x < lastx && firsty < y && y < lasty

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
