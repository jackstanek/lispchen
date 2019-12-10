-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine

import qualified Data.Map.Lazy as Map (empty)
import Data.Maybe (isJust)

import Ast
import Parser
import Eval

process :: String -> String
process input =
  let result = do
        ast <- parseSexp input
        eval ast in
    case result of
      Just sexp -> reprSexp sexp
      Nothing -> "error occurred"

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
