-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval (eval) where

import qualified Data.Map.Lazy as Map

import Ast

type Env = Map.Map Symbol Sexp

baseEnv :: Env
baseEnv = Map.empty -- TODO: Add base functions

truthy :: Sexp -> Bool
truthy s = case s of
  BoolVal b -> b
  IntVal 0 -> False
  StringVal "" -> False
  Nil -> False
  _ -> True

evalFnCall :: Env -> Symbol -> [Sexp] -> Maybe Sexp
evalFnCall env fn args =
  Nothing
  -- case fn of
    -- Symbol "if" ->
      -- if length args /= 3
      -- then Nothing
      -- else
        -- do
          -- condition' <- evalSexp env condition
          -- if truthy condition'
            -- then evalSexp env then'
            -- else evalSexp env else'
    -- _ -> Nothing

evalSexp :: Env -> Sexp -> Maybe Sexp
evalSexp env sexp =
  case sexp of
    SymbolVal s -> Map.lookup s env
    Quoted s -> Just s
    Cons (SymbolVal fn:args) -> evalFnCall env fn args
    _ -> Just sexp


eval :: Sexp -> Maybe Sexp
eval = evalSexp baseEnv
