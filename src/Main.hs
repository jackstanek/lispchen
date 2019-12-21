-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine
import System.Environment (getArgs)

import qualified Data.Map.Lazy as Map (empty)

import Ast
import Parser
import Eval

process :: String -> String
process input =
  case parseSexp input >>= eval of
    Left e -> "error: " ++ e
    Right sexp -> "=> " ++ reprSexp sexp

runRepl :: IO ()
runRepl =
  HLine.runInputT HLine.defaultSettings loop
  where
    loop :: HLine.InputT IO ()
    loop = do
      lineInput <- HLine.getInputLine "lispchen> "
      case lineInput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do
          HLine.outputStrLn $ process line
          loop

main :: IO ()
main = getArgs >>= \args ->
  case args of
    [] -> runRepl
    _ -> putStrLn "too many arguments"
