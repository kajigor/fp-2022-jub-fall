
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Reductions
import Lambda
import Data.GI.Base
import System.Posix.Unistd
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects as Objects


reduce :: Maybe (String, Lambda.Lambda String) -> Maybe (Lambda.Lambda String) 
reduce res = do 
  str <- res
  ans <- (reductor NormalOrder (snd str))
  return ans

buttonClickHandler :: Objects.Entry -> Gtk.Label -> IO ()
buttonClickHandler input output =
  do
    str <- Gtk.entryGetText input
    let x = Text.unpack str
    let lambda = (runParser exprParser x)
    let lambda_red = reduce lambda
    Gtk.labelSetText output (Text.pack (show lambda_red))

main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [#title := "Reduction"]
  on win #destroy Gtk.mainQuit
  
  #resize win 640 480

  msg <- new Objects.Entry []

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add win box

  #packStart box msg True False 10

  text <- new Gtk.Label [#label := "0"]
  #packStart box text True False 10

  btn <- new Gtk.Button [ #label := "Click me!" ]
  
  #packStart box btn False False 10

  on btn #clicked (buttonClickHandler msg text)-- [ #label := "Clicked"])

  #showAll win

  Gtk.main
