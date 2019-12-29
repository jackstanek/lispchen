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

evalFnCall :: Env -> Sexp -> [Sexp] -> Either String Sexp
evalFnCall env (Lambda arglist body) args =
  if length arglist /= length args then
    Left "wrong number of arguments passed"
  else
    evalSexp fnScope body
  where fnScope = Map.union (Map.fromList $ zip arglist args) env

evalFnCall env (Let bindings body) args =
  resolvedFn >>= \f -> evalFnCall env f args
  where resolvedFn = evalSexp env (Let bindings body)

evalFnCall env (SymbolVal (Symbol name)) args =
  case Map.lookup (Symbol name) env of
    Just s -> evalFnCall env s args
    Nothing -> Left $ "unbound function " ++ name

evalFnCall env s args = Left $ "called uncallable object " ++ reprSexp s

evalSexp :: Env -> Sexp -> Either String Sexp
evalSexp env sexp =
  case sexp of
    SymbolVal s ->
      case Map.lookup s env of
        Just v -> Right v
        Nothing -> Left $ "unbound " ++ show s
    Quoted s -> Right s
    If cond then' else' ->
      ev cond >>= branch . truthy
        where branch True = ev then'
              branch False = ev else'
    Let bindings body ->
      evalSexp (Map.union newVars env) body
      where newVars = Map.fromList bindings
    Cons (fn:args) ->
      evalFnCall env fn args

    _ -> Right sexp
  where ev = evalSexp env

eval :: Sexp -> Either String Sexp
eval = evalSexp baseEnv
