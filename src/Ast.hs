module Ast (Sexp(IntVal, StringVal, SymbolVal, BoolVal, Nil, Quoted, Cons),
            Symbol(Symbol), symval,
            reprSexp, testSexp) where

import Data.Char (toLower)
import Data.List (intersperse)

newtype Symbol = Symbol String
  deriving (Show, Eq, Ord)

data Sexp = IntVal Integer
          | StringVal String
          | SymbolVal Symbol
          | BoolVal Bool
          | Quoted Sexp
          | Cons [Sexp]
          | If Sexp Sexp Sexp
          | Let [(Symbol, Sexp)] Sexp
          | Lambda [Symbol] Sexp
          | Nil

  deriving (Show, Eq)

symval = SymbolVal . Symbol

testSexp = Cons [symval "foo", symval "bar", IntVal 420]

concatSpaces = concat . intersperse " "

-- Get a representation of a
reprSexp :: Sexp -> String
reprSexp (Cons contents) = "(" ++ (concatSpaces $ map reprSexp contents) ++ ")"

reprSexp atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal (Symbol s) -> s
    BoolVal b -> map toLower $ show b
    Quoted s -> reprSexp s
    Nil -> "nil"
    If _ _ _ -> "#(if ...)"
    Let _ _ -> "#(let ...)"
    Lambda _ _ -> "#(lambda ...)"
