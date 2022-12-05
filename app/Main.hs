module Main (main) where

import Minesweeper
import System.Random
import Graphics.UI.Gtk

main :: IO ()
main = do
  g <- newStdGen

  let fieldPair = generateField 10 10 10 (randomRs (0, 2147483647) g)
  let field = fst fieldPair
  let rndLst = snd fieldPair

  initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI

