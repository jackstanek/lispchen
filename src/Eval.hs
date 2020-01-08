-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval where

import qualified Data.Map.Lazy as Map

import ConcreteSyntax

type Env = Map.Map Symbol Sexp
data EvalTree = Val Sexp
              | If EvalTree EvalTree EvalTree
              | Let Env EvalTree
              | FnCall [(Symbol, EvalTree)] EvalTree
              | Closure Env EvalTree

baseEnv :: Env
baseEnv = Map.empty -- TODO: Add base functions

truthy :: Sexp -> Bool
truthy s = case s of
  IntVal 0 -> False
  StringVal "" -> False
  Nil -> False
  _ -> True
