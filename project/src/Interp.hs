-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

import Types

eval :: Expr -> Env -> Int -- You will almost certainly need to change this to use your own Value type.
eval (ENum i) _ = i
eval (EVar name) env = 
  case M.lookup name env of
    Nothing -> error $ "Variable " ++ name ++ " not defined"
    Just val -> val
eval (EAp (EAp (EVar "+") e1) e2) env = eval e1 env + eval e2 env
eval (EAp (EAp (EVar "-") e1) e2) env = eval e1 env - eval e2 env
eval (EAp (EAp (EVar "*") e1) e2) env = eval e1 env * eval e2 env
eval (EAp (EAp (EVar "/") e1) e2) env = 
  let a = eval e1 env
      b = eval e2 env
  in if b == 0 then error "Division by zero"
     else if a `mod` b == 0 then a `div` b
     else error "Division result is not an integer"
eval (EAp (EAp (EVar "<") e1) e2) env = if eval e1 env < eval e2 env then 1 else 0
eval (EAp (EAp (EVar ">") e1) e2) env = if eval e1 env > eval e2 env then 1 else 0
eval (EAp (EAp (EVar "<=") e1) e2) env = if eval e1 env <= eval e2 env then 1 else 0
eval (EAp (EAp (EVar ">=") e1) e2) env = if eval e1 env >= eval e2 env then 1 else 0
eval (EAp (EAp (EVar "==") e1) e2) env = if eval e1 env == eval e2 env then 1 else 0
eval (EAp (EAp (EVar "~=") e1) e2) env = if eval e1 env /= eval e2 env then 1 else 0
eval (EAp (EAp (EVar "&") e1) e2) env = if eval e1 env /= 0 && eval e2 env /= 0 then 1 else 0
eval (EAp (EAp (EVar "|") e1) e2) env = if eval e1 env /= 0 || eval e2 env /= 0 then 1 else 0
eval expr env = error $ "Cannot evaluate expression: " ++ show expr

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_,[],mainBody) ->
      let env = buildEnv prog
          result = eval mainBody env
       in show result

buildEnv :: Core -> Env
buildEnv prog = M.fromList [(name, eval body env) | (name, [], body) <- M.elems prog, name /= "main"]
  where env = buildEnv prog
  














