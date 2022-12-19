
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Reductions
import Lambda
import Data.Maybe
import Data.GI.Base
import Control.Exception
import System.Posix.Unistd
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects as Objects
import Text.Read

reduceIfOkWithStrategy :: Maybe (String, Lambda.Lambda String) -> Strategy -> String -> String
reduceIfOkWithStrategy Nothing strategy str = 
  ("Could not parse " ++ str)
reduceIfOkWithStrategy (Just ("", lambda)) strategy str = 
  (unlines (map show (reduce_list strategy lambda [])))
reduceIfOkWithStrategy (Just (remain, lambda)) strategy str = 
  ("Could not parse " ++ remain)

reduceIfOk :: Maybe (String, Lambda.Lambda String) -> (Maybe Strategy) -> String -> String
reduceIfOk lambda Nothing str = "Incorrect strategy"
reduceIfOk lambda (Just strategy) str = reduceIfOkWithStrategy lambda strategy str

buttonClickHandler :: Objects.Entry -> Objects.Entry -> Gtk.Label -> IO ()
buttonClickHandler input msg_type output =
  do
    str <- Gtk.entryGetText input
    type_str <- Gtk.entryGetText msg_type
    let x = Text.unpack str
    let type_ = readMaybe (Text.unpack type_str) 
    let lambda = (runParser exprParser x)
    let response = reduceIfOk lambda type_ x
    Gtk.labelSetText output (Text.pack response)

main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [#title := "Reduction"]
  on win #destroy Gtk.mainQuit
  
  #resize win 640 480

  msg <- new Objects.Entry []

  msg_type <- new Objects.Entry []

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add win box

  #packStart box msg True False 10
  #packStart box msg_type True False 10

  text <- new Gtk.Label [#label := "0"]
  #packStart box text True False 10

  btn <- new Gtk.Button [ #label := "Click me!" ]
  
  #packStart box btn False False 10

  on btn #clicked (buttonClickHandler msg msg_type text)-- [ #label := "Clicked"])

  #showAll win

  Gtk.main
