-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine

import qualified Data.Map.Lazy as Map (empty)

import Ast
import Parser
import Eval

process :: String -> String
process input =
  let result = do
        parseResult <- parseSexp input
        evalSexp Map.empty parseResult
  in case result of
       Just sexp -> show result
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
          HLine.outputStrLn $ process line
          loop
