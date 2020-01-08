module ConcreteSyntax where

newtype Symbol = Symbol String
  deriving (Show, Eq)

data Sexp = IntVal Integer
          | StringVal String
          | SymbolVal Symbol
          | Cons [Sexp]
          | Quoted Sexp
          | Nil
  deriving (Show, Eq)
