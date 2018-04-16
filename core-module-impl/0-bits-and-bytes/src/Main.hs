module Main where

import BitLanguage (evalToSteps)
import Parser
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings (outputStrLn introWelcomeMessage >> loop)
  where
    loop :: InputT IO ()
    loop = do
      userInput <- getInputLine "> "
      case userInput of
          Nothing -> return ()
          Just ":quit" -> return ()
          Just input -> (case parseBitLangExpr input of
            Left     _ -> outputStrLn "Syntax error. Please check your statement again."
            Right expr -> mapM_ outputStrLn (evalToSteps expr)) >> loop

introWelcomeMessage :: String
introWelcomeMessage = unlines [
    "",
    "Welcome to the bits and bytes lab!",
    "",
    "In this lab, we're going to be using this little read evaluate print loop (or simply referred to as REPL) to explore bitwise operators.",
    "",
    "Before we start exploring however, do note a couple of things: ",
    "\t- To exit out of this application, type in ':quit'.",
    "\t- Please feel free to give feedback! Submit an issue here: https://github.com/gestone/ada-cs-curriculum-booster-packs/issues if you have any feedback about this lab.",
    "",
    "Now you might be wondering, why explore these concepts in a REPL like this? Well, we could demonstrate a lot of these same concepts in other languages like Ruby,\
    \ but this custom REPL is built specifically for showing step by step what the computer is doing to bitwise operations easier to understand.",
    ""
  ]
