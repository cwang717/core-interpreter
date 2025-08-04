-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Data.Char (isUpper)

import Types

isTrue :: Value -> Bool
isTrue (VNum n) = n /= 0
isTrue (VFun _ _ _) = True
isTrue (VPack _ _) = True

isConstructor :: String -> Bool
isConstructor (c:_) = isUpper c
isConstructor [] = False

eval :: Expr -> Env -> Value
eval (ENum i) _ = VNum i
eval (ELam params body) env = VFun params body env
eval (EPack tag arity) env = VPack tag []
eval (EVar name) env = 
  case M.lookup name env of
    Nothing -> error $ "Variable " ++ name ++ " not defined"
    Just val -> val

eval (ECase scrutinee alternatives) env =
  case eval scrutinee env of
    VPack tag values ->
      case findMatchingAlt tag alternatives of
        Nothing -> error $ "No matching alternative for constructor " ++ show tag
        Just (_, vars, body) ->
          if length vars == length values
          then let env' = foldl (\acc (var, val) -> M.insert var val acc) env (zip vars values)
               in eval body env'
          else error $ "Arity mismatch: expected " ++ show (length vars) ++ " but got " ++ show (length values)
    _ -> error "Case scrutinee must be a constructor or number"
  where
    findMatchingAlt tag [] = Nothing
    findMatchingAlt tag ((altTag, vars, body):rest)
      | tag == altTag = Just (altTag, vars, body)
      | otherwise = findMatchingAlt tag rest

-- Put all specific binary operator patterns FIRST
eval (EAp (EAp (EVar "+") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (a + b)
    _ -> error "Addition requires numbers"
eval (EAp (EAp (EVar "-") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (a - b)
    _ -> error "Subtraction requires numbers"
eval (EAp (EAp (EVar "*") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (a * b)
    _ -> error "Multiplication requires numbers"
eval (EAp (EAp (EVar "/") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> 
      if b == 0 then error "Division by zero"
      else if a `mod` b == 0 then VNum (a `div` b)
      else error "Division result is not an integer"
    _ -> error "Division requires numbers"
eval (EAp (EAp (EVar "<") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a < b then 1 else 0)
    _ -> error "Comparison requires numbers"
eval (EAp (EAp (EVar ">") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a > b then 1 else 0)
    _ -> error "Comparison requires numbers"
eval (EAp (EAp (EVar "<=") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a <= b then 1 else 0)
    _ -> error "Comparison requires numbers"
eval (EAp (EAp (EVar ">=") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a >= b then 1 else 0)
    _ -> error "Comparison requires numbers"
eval (EAp (EAp (EVar "==") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a == b then 1 else 0)
    _ -> error "Equality requires numbers"
eval (EAp (EAp (EVar "~=") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (VNum a, VNum b) -> VNum (if a /= b then 1 else 0)
    _ -> error "Inequality requires numbers"
eval (EAp (EAp (EVar "&") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (a, b) -> VNum (if isTrue a && isTrue b then 1 else 0)
eval (EAp (EAp (EVar "|") e1) e2) env = 
  case (eval e1 env, eval e2 env) of
    (a, b) -> VNum (if isTrue a || isTrue b then 1 else 0)
eval (ELet isRec bindings body) env = 
  let env' = if isRec 
             then M.union (M.fromList [(name, eval expr env') | (name, expr) <- bindings]) env
             else foldl (\acc (name, expr) -> M.insert name (eval expr acc) acc) env bindings
  in eval body env'

eval (EAp func arg) env = 
  case eval func env of
    VPack tag values ->
      let argVal = eval arg env
      in VPack tag (values ++ [argVal])
    VFun (param:params) body closureEnv ->
      let argVal = eval arg env
          env' = M.insert param argVal closureEnv
      in if null params
         then eval body env'
         else VFun params body env'
    VFun [] _ _ -> error "Cannot apply function with no parameters"
    VNum _ -> error "Cannot apply non-function value"

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> 
      let allDecls = M.keys prog
      in error $ "Supercombinator main not defined. Declarations: " ++ unwords allDecls
    Just (_,[],mainBody) ->
      let env = buildEnv prog
          result = eval mainBody env
       in case result of
            VNum n -> show n
            _ -> error "Main must return a number"
    Just (_, params, _) ->
      error $ "Main function should have no parameters, but has: " ++ show params

buildEnv :: Core -> Env
buildEnv prog = env
  where env = M.fromList [(name, if null params 
                                 then eval body env 
                                 else VFun params body env) 
                         | (name, params, body) <- M.elems prog]
    