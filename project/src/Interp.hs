-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

import Types

isTrue :: Value -> Bool
isTrue (VNum n) = n /= 0
isTrue (VFun _ _ _) = True

eval :: Expr -> Env -> Value
eval (ENum i) _ = VNum i
eval (ELam params body) env = VFun params body env

eval (EVar name) env = 
  case M.lookup name env of
    Nothing -> error $ "Variable " ++ name ++ " not defined"
    Just val -> val

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
    VFun (param:params) body closureEnv ->
      let argVal = eval arg env
          env' = M.insert param argVal closureEnv
      in if null params
         then eval body env'
         else VFun params body env'
    VFun [] _ _ -> error "Cannot apply function with no parameters"
    VNum _ -> error "Cannot apply non-function value"

eval expr env = error $ "Cannot evaluate expression: " ++ show expr

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
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
