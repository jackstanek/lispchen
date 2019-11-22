-- Copyright 2019, Jack Stanek. All rights reserved.

import System.Console.Haskeline
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec

data Atom = IntVal Integer
          | StringVal String
          | SymbolVal String
          | TrueLit
          | Nil
          | Quoted Sexp

data Sexp = Value Atom | Cons Sexp Sexp

testSexp = (Value (Quoted (Cons (Value (SymbolVal "yolo"))
             (Cons (Value (IntVal 4)) (Value (IntVal 20))))))


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
          outputStrLn . ("=> " ++) . reprSexp $ testSexp
          loop

evalSexp :: Sexp -> Sexp
evalSexp (Value a) = Value a
evalSexp (Cons left right) =
  Cons left right


-- Get representation of an atom.
reprAtom :: Atom -> String
reprAtom atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal s -> s
    TrueLit -> "t"
    Nil -> "nil"
    Quoted s -> case s of
                  Value v -> reprAtom v
                  otherwise -> "'" ++ reprSexp s

-- Get a representation of the contents of a cons.
innerTerms :: Sexp -> String
innerTerms (Value v) = reprAtom v
innerTerms (Cons left right) = reprSexp left ++ " " ++ innerTerms right

-- Get a representation of a
reprSexp :: Sexp -> String
reprSexp (Value atom) = reprAtom atom
reprSexp (Cons left right) =
  "(" ++ reprSexp left ++ " " ++ innerTerms right ++ ")"
