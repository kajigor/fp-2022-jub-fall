module Gui where

import Graphics.UI.Gtk
import Minesweeper

main :: IO ()
main = do
    initGUI
    window <- windowNew
    widgetShowAll window
    mainGui