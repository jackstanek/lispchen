module Ast (Atom (IntVal, StringVal, SymbolVal, TrueLit, Nil, Quoted),
            Sexp (Value, Cons),
            testSexp, reprSexp) where

data Atom = IntVal Integer
          | StringVal String
          | SymbolVal String
          | TrueLit
          | Nil
          | Quoted Sexp

data Sexp = Value Atom | Cons Sexp Sexp

testSexp = (Value (Quoted (Cons (Value (SymbolVal "yolo"))
             (Cons (Value (IntVal 4)) (Value (IntVal 20))))))

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
