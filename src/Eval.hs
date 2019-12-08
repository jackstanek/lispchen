-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval (evalSexp) where

import qualified Data.Map.Lazy as Map

import Ast

evalSexp :: Map.Map Symbol Sexp -> Sexp -> Maybe Sexp
evalSexp env (Cons left right) = Just $ Cons left right
evalSexp env (atom) = Just atom
