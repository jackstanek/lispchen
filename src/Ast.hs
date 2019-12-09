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
          | Nil

  deriving (Show, Eq)

symval = SymbolVal . Symbol

testSexp = Cons [symval "foo", symval "bar", IntVal 420]

-- Get a representation of a
reprSexp :: Sexp -> String
reprSexp (Cons contents) = "(" ++ (concatSpaces $ map reprSexp contents) ++ ")"
  where concatSpaces = concat . intersperse " "

reprSexp atom =
  case atom of
    IntVal i -> show i
    StringVal s -> "\"" ++ s ++ "\""
    SymbolVal (Symbol s) -> s
    BoolVal b -> map toLower $ show b
    Nil -> "nil"
    Quoted s -> reprSexp s
