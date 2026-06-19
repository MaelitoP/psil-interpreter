{-# OPTIONS_GHC -Wall #-}

-- Bidirectional type checker: type synthesis (infer) and checking (check).
module Psil.Typecheck
  ( TEnv,
    tenv0,
    infer,
    check,
  )
where

import Control.Monad (foldM)
import Psil.Core (Error, Lexp (..), Ltype (..), Var)
import Psil.Eval (env0)

type TEnv = [(Var, Ltype)]

-- Values are irrelevant to type checking, so keep only the types from env0.
tenv0 :: TEnv
tenv0 = map (\(x, _, t) -> (x, t)) env0

-- Looks up the type of a variable.
tlookup :: [(Var, a)] -> Var -> Either Error a
tlookup [] x = Left ("Unknown variable: " ++ x)
tlookup ((x', t) : _) x | x == x' = Right t
tlookup (_ : env) x = tlookup env x

-- Typing rules: type synthesis.
infer :: TEnv -> Lexp -> Either Error Ltype
infer _ (Lnum _) = Right Lint
infer tenv (Lvar x) = tlookup tenv x
infer tenv (Lhastype e t) = check tenv e t >> Right t
infer tenv (Lcall e1 e2) = do
  ft <- infer tenv e1
  case ft of
    Larw t1 t2 -> check tenv e2 t1 >> Right t2
    _ -> Left ("Not a function: " ++ show ft)
infer tenv (Llet ds b) =
  -- Bindings may be mutually recursive: seed the environment from the
  -- declared types of annotated bindings so recursive references resolve.
  let annotated = [(v, t) | (v, Lhastype _ t) <- ds]
      baseEnv = annotated ++ tenv
      addBinding env (_, Lhastype _ _) = Right env
      addBinding env (v, e) = (\t -> (v, t) : env) <$> infer env e
      checkBinding fullEnv (_, Lhastype body t) = check fullEnv body t
      checkBinding _ _ = Right ()
   in do
        fullEnv <- foldM addBinding baseEnv ds
        mapM_ (checkBinding fullEnv) ds
        infer fullEnv b
infer tenv (Ltuple es) = Ltup <$> mapM (infer tenv) es
infer _ (Lfun _ _) = Left "Can't infer type of `fun`"
infer _ (Lif _ _ _) = Left "Can't infer type of `if`"
infer _ (Lfetch _ _ _) = Left "Can't infer type of `fetch`"

-- Typing rules: checking judgment.
check :: TEnv -> Lexp -> Ltype -> Either Error ()
check tenv (Lfun x body) (Larw t1 t2) = check ((x, t1) : tenv) body t2
check _ (Lfun _ _) t = Left ("Not a function type: " ++ show t)
check tenv (Lif e1 e2 e3) t = do
  check tenv e1 Lboo
  check tenv e2 t
  check tenv e3 t
check tenv (Lfetch tup xs e) t = do
  tupleType <- case tup of
    Ltuple _ -> do
      tt <- infer tenv tup
      case tt of
        Ltup list -> Right list
        _ -> Left "Not a tuple"
    Lvar var -> do
      tt <- tlookup tenv var
      case tt of
        Ltup list -> Right list
        _ -> Left "Not a tuple"
    _ -> Left "Not a tuple"
  if length tupleType /= length xs
    then
      Left
        ( "Tuple length and number of variables mismatch: "
            ++ show (length tupleType)
            ++ " != "
            ++ show (length xs)
        )
    else check (zip xs tupleType ++ tenv) e t
check tenv e t = do
  -- Fall back to synthesis and compare with the expected type.
  t' <- infer tenv e
  if t == t'
    then Right ()
    else Left ("Type mismatch: " ++ show t ++ " != " ++ show t')
