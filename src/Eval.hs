-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval (eval) where

import qualified Data.Map.Lazy as Map

import Data.Maybe

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

argsMatch :: [Sexp] -> Int -> Bool
argsMatch args len = len == length args

evalFnCall :: Env -> Symbol -> [Sexp] -> Maybe Sexp
evalFnCall env fn args = Nothing

evalSexp :: Env -> Sexp -> Maybe Sexp
evalSexp env sexp =
  case sexp of
    SymbolVal s -> Map.lookup s env
    Quoted s -> Just s
    If cond then' else' -> do
      cond' <- ev cond
      if truthy cond'
        then ev then'
        else ev else'
    Cons (SymbolVal fn:args) -> evalFnCall env fn args
    _ -> Just sexp
  where ev = evalSexp env

eval :: Sexp -> Maybe Sexp
eval = evalSexp baseEnv
