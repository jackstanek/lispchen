module Ast (Sexp(IntVal, StringVal, SymbolVal, BoolVal, Nil, Quoted, Cons),
            reprSexp, testSexp) where

import Data.Char (toLower)

data Sexp = IntVal Integer
          | StringVal String
          | SymbolVal String
          | BoolVal Bool
          | Quoted Sexp
          | Cons Sexp Sexp
          | Nil

  deriving (Show, Eq)

testSexp = (Cons (SymbolVal "yolo") (Cons (Cons (IntVal 420) (IntVal 69)) Nil))

-- Get a representation of the contents of a cons.
innerTerms :: Sexp -> String
innerTerms s =
  case s of
    Nil -> ""
    (Cons left Nil) -> reprSexp left
    (Cons left right) -> reprSexp left ++ " " ++ innerTerms right
    _ -> reprSexp s

-- Get a representation of a
reprSexp :: Sexp -> String
reprSexp (Cons left right) =
  let sep = if right /= Nil then " " else "" in
    "(" ++ reprSexp left ++ sep ++ innerTerms right ++ ")"

reprSexp atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal s -> s
    BoolVal b -> map toLower $ show b
    Nil -> "nil"
    Quoted s -> reprSexp s
