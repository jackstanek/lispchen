-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline
import System.IO
import System.Environment
import Text.ParserCombinators.ReadP

data Atom = IntVal Integer |
            StringVal String |
            SymbolVal String |
            TrueLit |
            Nil

data Sexp = Value Atom | Cons Atom Sexp

main :: IO ()
main =
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      lineInput <- getInputLine "lispchen> "
      case lineInput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do
          outputStrLn . ("=> " ++) . reprSexp . parseSexp $ line
          loop

parseSexp :: String -> Sexp
parseSexp str = Value (SymbolVal "foo")

reprAtom :: Atom -> String
reprAtom atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal s -> s
    TrueLit -> "t"
    Nil -> "nil"

reprSexp :: Sexp -> String
reprSexp (Value atom) = reprAtom atom
reprSexp (Cons atom sexp) =
  "(" ++ reprAtom atom ++ " " ++ innerTerms sexp ++ ")"
  where innerTerms s =
          case s of
            Value a -> reprAtom a
            Cons a s' -> reprAtom a ++ " " ++ innerTerms s'
