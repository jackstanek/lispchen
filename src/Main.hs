-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline as HLine
import System.Environment (getArgs)
import System.IO

import qualified Data.Map.Lazy as Map (empty)

import Parser
import Eval

process :: String -> String
process input =
  case parseSexp input of
    Left e -> "error: " ++ e
    Right sexp -> show sexp

runRepl :: IO ()
runRepl =
  HLine.runInputT HLine.defaultSettings loop
  where
    loop :: HLine.InputT IO ()
    loop = HLine.getInputLine "lispchen> " >>= \lineInput ->
      case lineInput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do
          HLine.outputStrLn $ "=> " ++ process line
          loop

runScript :: FilePath -> IO ()
runScript path = withFile path ReadMode $ \h ->
  hGetContents h >>= \script -> putStrLn $ process script

main :: IO ()
main = getArgs >>= \args ->
  case args of
    (path:[]) -> runScript path
    [] -> runRepl
    _ -> putStrLn "too many arguments"
