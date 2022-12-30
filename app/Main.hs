{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Spock as Spock
import Web.Spock.Config as Spock
import Data.Aeson as A
import Data.Text 
import Data.String
import Data.IORef
import Parser
import Reductions
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Semigroup ((<>))
import Lambda
import Web.Spock.Lucid (lucid)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Lucid

data Task = Task { lamb :: Text,  strat :: Text, evaluating :: [String]}
newtype ServerState = ServerState { tasks :: IORef [Task] }


parse_evalutating :: [String] -> String
parse_evalutating []  = ""
parse_evalutating (a : []) = a
parse_evalutating (a : xs) = a ++ "  --->  " ++ (parse_evalutating xs)



evalBySteps :: Maybe (Lambda String) -> String -> [String]
evalBySteps Nothing _ = ["Can't parse lambda term"]
evalBySteps (Just lambda) strat_name = case red_lst of 
                                    Nothing -> ["Can't reduce this lambda"]
                                    Just x ->  (Prelude.map show x)                
                                    where
                                      red_lst = reductionList strat lambda [lambda]
                                      strat | strat_name == "Normal Order" = NormalOrder
                                            | strat_name == "Applicative Order" = ApplicativeOrder
                                            | strat_name == "Call By Name" = CallByName
                                            | strat_name == "Call By Value" = CallByValue
                                  
                                      

-- λx.λy.x y
type Server a = SpockM () () ServerState a
app :: Server () 
app = do 
  get root $ do 
    tasks' <- getState >>= (liftIO . readIORef . tasks)
    lucid $ do
      h1_ "Terms:"
      p_ $ forM_ tasks' $ \task -> li_ $ do
        b_ "Lambda term: "
        toHtml (lamb task)
        b_ " Strategy: "
        toHtml (strat task)
        b_ " Eval: "
        toHtml (parse_evalutating (evaluating task))
        
      h2_ "Add new term"
      form_ [method_ "post"] $ do
        label_ $ do
          "Lambda term: : "
          input_ [name_ "lambda"]
        label_ $ do
          "Strategy: "
          select_ [name_ "strategy"] $ do 
            option_ [value_ "Normal Order"] "Normal Order"
            option_ [value_ "Applicative Order"] "Applicative Order"
            option_ [value_ "Call By Value"] "Call By Value"
            option_ [value_ "Call By Name"] "Call By Name"
            
        input_ [type_ "submit", value_ "Add Term"]  
  post root $ do
    lambda <- param' "lambda"
    strat <- param' "strategy"
    tasksRef <- tasks <$> getState
    liftIO $ atomicModifyIORef' tasksRef $ \tasks ->
      (tasks <> [Task lambda strat (evalBySteps (parseMaybe parseLambda (unpack (strip lambda))) (unpack strat))] , ())
    redirect "/"  




main :: IO ()
main = do
  server_state <- ServerState <$> newIORef [Task "(λx.x) y" "Call By Value" (evalBySteps (parseMaybe parseLambda "(λx.x) y") "Call By Value")]
  -- There are space to add readed from file terms ^
  spockCfg <- defaultSpockCfg () PCNoDatabase server_state
  runSpock 3000 $ (spock spockCfg app)
