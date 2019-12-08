-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine

import Ast
import Parser
import Eval

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
          let result = case parseSexp line of
                         Just sexp -> reprSexp $ evalSexp sexp
                         Nothing -> "parse error" in
            HLine.outputStrLn result
          loop
