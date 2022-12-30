
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

reduceIfOkSteps :: Maybe (String, Lambda.Lambda String) -> Strategy  -> String
reduceIfOkSteps (Just ("", lambda)) strategy = checkIfTerminated list_terms 
  where list_terms = (map show (reduce_list strategy lambda 100 []))
        checkIfTerminated list_terms | (length list_terms) == 100 = (unlines list_terms) ++ "Didn't terminate\n"
                                     | otherwise = (unlines list_terms) 
reduceIfOkSteps _ _ =  ("Could not parse lambda")

reduceIfOk :: Maybe (String, Lambda.Lambda String) -> (Maybe Strategy) -> String
reduceIfOk lambda Nothing = "Incorrect strategy"
reduceIfOk lambda (Just strategy) = reduceIfOkSteps lambda strategy

buttonClickHandlerUntilConvergence :: Objects.Entry -> Objects.Entry -> Gtk.Label -> IO ()
buttonClickHandlerUntilConvergence input msg_type output =
  do
    str <- Gtk.entryGetText input
    type_str <- Gtk.entryGetText msg_type
    let x = Text.unpack str
    let type_ = readMaybe (Text.unpack type_str) 
    let lambda = (runParser exprParser x)
    let response = reduceIfOk lambda type_
    Gtk.labelSetText output (Text.pack response)

main :: IO ()
main = do
  Gtk.init Nothing

  scrolledWin <- new Gtk.ScrolledWindow []

  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 1024
                        , #defaultHeight := 768
                        , #child := scrolledWin ]
  on win #destroy Gtk.mainQuit
  
  term_message <- new Gtk.Label [#label := ""]
  Gtk.labelSetText term_message "Print term"

  msg <- new Objects.Entry []

  type_message <- new Gtk.Label [#label := ""]
  Gtk.labelSetText type_message "Print type (choose from CallByValue | CallByName | NormalOrder | ApplicativeOrder)"

  msg_type <- new Objects.Entry []

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add scrolledWin box

  #packStart box term_message True False 0
  #packStart box msg True False 10
  #packStart box type_message True False 0
  #packStart box msg_type True False 10

  text <- new Gtk.Label [#label := ""]
  #packStart box text True False 10

  btnUntilConvergence <- new Gtk.Button [ #label := "Show steps until convergence" ]
  
  #packStart box btnUntilConvergence False False 10

  on btnUntilConvergence #clicked (buttonClickHandlerUntilConvergence msg msg_type text)

  #showAll win

  Gtk.main





-- {-# LANGUAGE OverloadedLabels  #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- module Main where

-- import Parser
-- import Reductions
-- import Lambda
-- import Data.Maybe
-- import Data.GI.Base
-- import Control.Exception
-- import System.Posix.Unistd
-- import qualified Data.Text as Text
-- import qualified GI.Gtk as Gtk
-- import qualified GI.Gtk.Objects as Objects
-- import Text.Read

-- reduceIfOkSteps :: Maybe (String, Lambda.Lambda String) -> Strategy  -> String
-- reduceIfOkSteps (Just ("", lambda)) strategy = checkIfTerminated list_terms 
--   where list_terms = (map show (reduce_list strategy lambda 100 []))
--         checkIfTerminated list_terms | (length list_terms) == 100 = (unlines list_terms) ++ "Didn't terminate\n"
--                                      | otherwise = (unlines list_terms) 
-- reduceIfOkSteps _ _ =  ("Could not parse lambda")


-- reduceIfOkResult :: Maybe (String, Lambda.Lambda String) -> Strategy -> String 
-- reduceIfOkResult (Just ("", lambda)) strategy = checkIfTerminated list_terms 
--   where list_terms = (map show (reduce_list strategy lambda 100 []))
--         checkIfTerminated list_terms | (length list_terms) == 100 = "Didn't terminate\n"
--                                      | otherwise = show (last list_terms) 
-- reduceIfOkResult _ _ =  ("Could not parse lambda")


-- reduceIfOkKSteps :: Maybe (String, Lambda.Lambda String) -> Strategy -> Maybe Int -> String 
-- reduceIfOkResult (Just ("", lambda)) strategy (Just k) = checkIfTerminated list_terms 
--   where list_terms = (map show (reduce_list strategy lambda k []))
--         checkIfTerminated list_terms = (unlines list_terms)
-- reduceIfOkKSteps _ _ Nothing = ("Could not parse number of steps")
-- reduceIfOkKSteps _ _ _ =  ("Could not parse lambda")


-- reduceIfOk :: Maybe (String, Lambda.Lambda String) -> (Maybe Strategy) -> String
-- reduceIfOk lambda Nothing = "Incorrect strategy"
-- reduceIfOk lambda (Just strategy) = reduceIfOkSteps lambda strategy Nothing

-- buttonClickHandlerUntilConvergence :: Objects.Entry -> Objects.Entry -> Gtk.Label -> IO ()
-- buttonClickHandlerUntilConvergence input msg_type output =
--   do
--     str <- Gtk.entryGetText input
--     type_str <- Gtk.entryGetText msg_type
--     let x = Text.unpack str
--     let type_ = readMaybe (Text.unpack type_str) 
--     let lambda = (runParser exprParser x)
--     let response = reduceIfOk lambda type_
--     Gtk.labelSetText output (Text.pack response)

-- main :: IO ()
-- main = do
--   Gtk.init Nothing

--   scrolledWin <- new Gtk.ScrolledWindow []

--   win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
--                         , #iconName := "applications-haskell"
--                         , #defaultWidth := 1024
--                         , #defaultHeight := 768
--                         , #child := scrolledWin ]
--   on win #destroy Gtk.mainQuit
  
--   term_message <- new Gtk.Label [#label := ""]
--   Gtk.labelSetText term_message "Print term"

--   msg <- new Objects.Entry []

--   type_message <- new Gtk.Label [#label := ""]
--   Gtk.labelSetText type_message "Print type (choose from CallByValue | CallByName | NormalOrder | ApplicativeOrder)"

--   msg_type <- new Objects.Entry []

--   box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
--   #add scrolledWin box

--   -- cont <- new Objects.Container []

--   -- btnFile <- Gtk.FileChooserButton []

--   #packStart box term_message True False 0
--   #packStart box msg True False 10
--   #packStart box type_message True False 0
--   #packStart box msg_type True False 10

--   text <- new Gtk.Label [#label := ""]
--   #packStart box text True False 10

--   btnUntilConvergence <- new Gtk.Button [ #label := "Show steps until convergence" ]
--   btnResult <- new Gtk.Button [ #label := "Show result of convergence" ]
--   btnKSteps <- new Gtk.Button [ #label := "Show k steps of reduction" ]
  
--   #packStart box btnUntilConvergence False False 10
--   #packStart box btnResult False False 10
--   #packStart box btnKSteps False False 10
--   -- #packStart box btnFile False False 10

--   on btnUntilConvergence #clicked (buttonClickHandlerUntilConvergence msg msg_type text)
--   --on btnResult #clicked ()

--   #showAll win

--   Gtk.main
