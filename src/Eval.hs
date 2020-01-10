-- Copyright 2019, Jack Stanek. All rights reserved.


module Eval where

import Control.Monad (mapM)
import qualified Data.Map.Lazy as Map

import ConcreteSyntax

type Env = Map.Map Symbol EvalTree
data EvalTree = Val Sexp
              | If EvalTree EvalTree EvalTree
              | Let [(Symbol, EvalTree)] EvalTree
              | FnCall EvalTree [EvalTree]
              | Closure Env EvalTree
  deriving Show

baseEnv :: Env
baseEnv = Map.empty -- TODO: Add base functions

truthy :: Sexp -> Bool
truthy s = case s of
  IntVal 0 -> False
  StringVal "" -> False
  Nil -> False
  _ -> True

buildAST :: Sexp -> Either String EvalTree
buildAST (Cons sexps) = case sexps of
  [SymbolVal (Symbol "let"), bindings, body] -> do
    bound <- buildBindings bindings
    bodyAST <- buildAST body
    Right $ Let bound bodyAST
  (SymbolVal (Symbol "let")):_ -> Left $ "invalid let expression"
  fn:args -> mapM buildAST args >>= \argASTs -> Right $ FnCall (Val fn) argASTs
  _ -> Left "not implemented"
  where buildBindings :: Sexp -> Either String [(Symbol, EvalTree)]
        buildBindings (Cons bindings) = mapM bindSym bindings
        buildBindings _ = Left "invalid let expression"
        bindSym (Cons [SymbolVal sym, val]) = buildAST val >>= \ast -> Right (sym, ast)
        bindSym _ = Left "invalid binding in let expression"

buildAST sexp = Right $ Val sexp
