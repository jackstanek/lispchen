-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine

import qualified Data.Map.Lazy as Map (empty)

import Ast
import Parser
import Eval

process :: String -> String
process input =
  case parseSexp input of
    Just sexp ->
      case eval sexp of
        Just result -> reprSexp result
        Nothing -> "runtime error"
    Nothing -> "parse error"

main :: IO ()
main =
  HLine.runInputT HLine.defaultSettings loop
  where
    loop :: HLine.InputT IO ()
    loop = do
      lineInput <- HLine.getInputLine "lispchen> "
      case lineInput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do
          HLine.outputStrLn $ "=> " ++ process line
          loop
