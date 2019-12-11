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

evalFnCall :: Env -> Sexp -> [Sexp] -> Either String Sexp
evalFnCall env (Lambda arglist body) args =
  evalSexp fnScope body
  where fnScope = Map.union (Map.fromList $ zip arglist args) env
evalFnCall env (SymbolVal (Symbol name)) args =
  case Map.lookup (Symbol name) env of
    Just s -> evalFnCall env s args
    Nothing -> Left $ "unbound function " ++ name
evalFnCall env s args = Left $ "called uncallable object " ++ reprSexp s

evalSexp :: Env -> Sexp -> Either String Sexp
evalSexp env sexp =
  case sexp of
    SymbolVal s ->
      let val = Map.lookup s env in
        case val of
          Just v -> Right v
          Nothing -> Left $ "unbound " ++ show s
    Quoted s -> Right s
    If cond then' else' -> do
      cond' <- ev cond
      if truthy cond'
        then ev then'
        else ev else'
    Let bindings body ->
      evalSexp (Map.union newVars env) body
      where newVars = Map.fromList bindings
    Cons (fn:args) ->
      evalFnCall env fn args

    _ -> Right sexp
  where ev = evalSexp env

eval :: Sexp -> Either String Sexp
eval = evalSexp baseEnv
