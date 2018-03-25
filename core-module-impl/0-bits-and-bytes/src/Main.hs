module Main where

import BitLanguage (evalToSteps)
import Control.Monad (unless)
import Parser
import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the bits and bytes lab!"
  startMainLoop

startMainLoop :: IO ()
startMainLoop = do
  putStr "> "
  hFlush stdout -- Because of Haskell laziness, we'll need to put this line in to get '>' to appear
  userInput <- getLine
  unless (userInput == ":quit") $ do
    case parseBitLangExpr userInput of
      Left  _    -> putStrLn "Syntax error. Please check your statement again."
      Right expr -> mapM_ putStrLn $ evalToSteps expr
    startMainLoop

