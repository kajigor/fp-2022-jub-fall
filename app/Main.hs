module Main (main) where

import Minesweeper
import System.Random
import Graphics.UI.Gtk

main :: IO ()
main = do
    g <- newStdGen

    initGUI
    window <- windowNew

    let rows = 10
    let cols = 10
    let bombs = 10

    let fieldPair = generateField rows cols bombs (randomRs (0, 2147483647) g)
    let field = fst fieldPair
    let rndLst = snd fieldPair

    set window [windowTitle := "Table", containerBorderWidth := 20,
                windowDefaultWidth := 150, windowDefaultHeight := 100]
    table   <- tableNew rows cols True
    containerAdd window table
    --button1 <- buttonNewWithLabel "On"
    --onClicked button1 (buttonSwitch button1)
    --tableAttachDefaults table button1 0 1 0 1
    --button2 <- buttonNewWithLabel "Off"
   -- onClicked button2 (buttonSwitch button2)
    --tableAttachDefaults table button2 1 2 0 1
    --button3 <- buttonNewWithLabel "Quit"
    --onClicked button3 mainQuit
    --tableAttachDefaults table button3 0 2 1 2
    let buttons = createButtonFieldTable field table
    onDestroy window mainQuit

    widgetShowAll window
    mainGUI

createButtonFieldTable :: FieldChars -> Table -> [[Button]]
createButtonFieldTable field table = createButtonFieldTableHelper field table 0 0 []
    where 
        createButtonFieldTableHelper field table x y currRow =
            if (x == (rows field)) then []
            else if (y == (cols field)) then currRow : (createButtonFieldTableHelper field table (x + 1) 0 [])
            else do
                button <- buttonNewWithLabel ""
                ret <- tableAttachDefaults table button y (y + 1) x (x + 1)
                (createButtonFieldTableHelper field table x (y + 1) (currRow ++ [button]))