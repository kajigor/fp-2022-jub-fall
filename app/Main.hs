module Main (main) where

import Minesweeper
import System.Random
import Graphics.UI.Gtk

data FieldButtons = [[BU]]

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
    buttons <- createButtonFieldTable field table
    onDestroy window mainQuit

    widgetShowAll window
    mainGUI

createButtonFieldTable :: FieldChars -> IO Table -> FieldButtons
createButtonFieldTable field table = createButtonFieldTableHelper field table 0 0 []
    where 
        createButtonFieldTableHelper field table x y currRow =
            if (x == rows) then []
            else if (y == cols) then currRow : (createButtonFieldTableHelper rows cols (x + 1) 0 [])
            else do
                button <- buttonNewWithLabel ""
                tableAttachDefaults table button2 cols (cols + 1) rows (rows + 1)
                (createButtonFieldTablesHelper rows cols x (y + 1) (currRow ++ [button]))