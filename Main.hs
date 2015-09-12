module Main where

import Control.Monad.Trans (liftIO)

import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.Builder (builderNew, builderAddFromFile, builderGetObject)
import qualified Graphics.Rendering.Cairo as Cairo


main :: IO ()
main = do
        GTK.initGUI

        builder <- builderNew
        builderAddFromFile builder "UI/life.glade"

        mainWin <- builderGetObject builder GTK.castToWindow "mainWin"
        mainWin `GTK.on` GTK.deleteEvent $ liftIO GTK.mainQuit >> return False

        GTK.widgetShowAll mainWin
        GTK.mainGUI
