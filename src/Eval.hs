-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval (evalSexp) where

import Ast

evalSexp :: Sexp -> Sexp
evalSexp (Value a) = Value a
evalSexp (Cons left right) =
  Cons left right
