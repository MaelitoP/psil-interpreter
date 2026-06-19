{-# OPTIONS_GHC -Wall #-}

-- Evaluator: reduces a core Lexp to a Value.
module Psil.Eval
  ( Value (..),
    Env,
    env0,
    eval,
  )
where

import Psil.Core (Lexp (..), Ltype (..), Var)

-- Values produced by the evaluator.
data Value
  = Vnum Int
  | Vbool Bool
  | Vtuple [Value]
  | Vfun (Maybe String) (Value -> Value)

instance Show Value where
  showsPrec p (Vnum n) = showsPrec p n
  showsPrec p (Vbool b) = showsPrec p b
  showsPrec p (Vtuple vs) = showValues "[" vs
    where
      showValues _ [] = showString "]"
      showValues sep (v : vs') =
        showString sep . showsPrec p v . showValues " " vs'
  showsPrec _ (Vfun (Just n) _) =
    showString "<fun " . showString n . showString ">"
  showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- Initial environment: the built-in functions and their types.
env0 :: Env
env0 =
  [ prim "+" (+) Vnum Lint,
    prim "-" (-) Vnum Lint,
    prim "*" (*) Vnum Lint,
    prim "/" div Vnum Lint,
    prim "=" (==) Vbool Lboo,
    prim ">=" (>=) Vbool Lboo,
    prim "<=" (<=) Vbool Lboo
  ]
  where
    prim name op cons typ =
      ( name,
        Vfun
          (Just name)
          ( \(Vnum x) ->
              Vfun
                Nothing
                (\(Vnum y) -> cons (x `op` y))
          ),
        Larw Lint (Larw Lint typ)
      )

-- Entry point for evaluation.
eval :: Env -> Lexp -> Value
eval env e =
  -- Split the env into names and values; types are unused at eval time.
  eval2 (map (\(x, _, _) -> x) env) e (map (\(_, v, _) -> v) env)

e2lookup :: [Var] -> Var -> Int -- Find position within environment
e2lookup env x = e2lookup' env 0
  where
    e2lookup' :: [Var] -> Int -> Int
    e2lookup' [] _ = error ("Unknown variable: " ++ show x)
    e2lookup' (x' : _) i | x == x' = i
    e2lookup' (_ : xs) i = e2lookup' xs (i + 1)

-------------- Main evaluation function.  -----------------------------------
-- Instead of one list of (Var, Value) pairs, the names (`senv`) and the
-- values (`venv`) are passed separately so that (eval2 senv e) returns a
-- function that is already done with `senv`.
eval2 :: [Var] -> Lexp -> ([Value] -> Value)
eval2 _ (Lnum n) = \_ -> Vnum n
eval2 senv (Lhastype e _) = eval2 senv e
eval2 senv (Lvar x) =
  -- Resolve the variable's position once, so a function returned by
  -- (eval2 senv v) does the name lookup only once however often it runs.
  let i = e2lookup senv x
   in (!! i)
eval2 senv (Lcall o a) = \venv ->
  let Vfun _ f = eval2' o
      n = eval2' a
      eval2' x = eval2 senv x venv
   in f n
eval2 senv (Lfun a e) =
  \venv -> Vfun Nothing (\v -> eval2 (a : senv) e (v : venv))
eval2 senv (Llet ds b) = \venv ->
  let (vars, exps) = unzip ds
      senv' = vars ++ senv
      venv' = map eval2' exps ++ venv
      eval2' v = eval2 senv' v venv'
   in eval2' b
eval2 senv (Lif e1 e2 e3) = \venv ->
  let eval2' x = eval2 senv x venv
   in case eval2' e1 of
        Vbool True -> eval2' e2
        _ -> eval2' e3
eval2 senv (Ltuple e) = \venv ->
  let eval2' v = eval2 senv v venv
   in Vtuple (map eval2' e)
eval2 senv (Lfetch tup vs e) = \venv ->
  let Vtuple tuplist = eval2 senv tup venv
      senv' = vs ++ senv
      venv' = tuplist ++ venv
   in eval2 senv' e venv'
