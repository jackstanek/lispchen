module Ast (Sexp(IntVal, StringVal, SymbolVal, BoolVal, Nil, Cons),
            reprSexp, testSexp) where

import Data.Char (toLower)

data Sexp = IntVal Integer
          | StringVal String
          | SymbolVal String
          | BoolVal Bool
          | Nil
          | Quoted Sexp
          | Cons Sexp Sexp
          | End

  deriving (Show, Eq)

testSexp = (Cons (SymbolVal "yolo") (Cons (Cons (IntVal 420) (IntVal 69)) End))

-- Get a representation of the contents of a cons.
innerTerms :: Sexp -> String
innerTerms (Cons left right) = reprSexp left ++ " " ++ innerTerms right
innerTerms atom = reprSexp atom

-- Get a representation of a
reprSexp :: Sexp -> String
reprSexp (Cons left right) =
  "(" ++ reprSexp left ++ " " ++ innerTerms right ++ ")"

reprSexp atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal s -> s
    BoolVal b -> map toLower $ show b
    Nil -> "nil"
    Quoted s -> "'" ++ reprSexp atom
    End -> ""
