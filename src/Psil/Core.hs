{-# OPTIONS_GHC -Wall #-}

-- Core language (typed) and lowering from s-expressions.
module Psil.Core
  ( Var,
    Error,
    Ltype (..),
    Lexp (..),
    s2l,
  )
where

import Psil.Reader (Sexp (..), showSexp)

type Var = String

data Ltype
  = Lint -- Int
  | Lboo -- Bool
  | Larw Ltype Ltype -- τ₁ → τ₂
  | Ltup [Ltype] -- tuple τ₁...τₙ
  deriving (Show, Eq)

data Lexp
  = Lnum Int -- Integer constant
  | Lvar Var -- Variable reference
  | Lhastype Lexp Ltype -- Type annotation
  | Lcall Lexp Lexp -- Function call (one argument)
  | Lfun Var Lexp -- Anonymous one-argument function
  | Llet [(Var, Lexp)] Lexp -- Possibly mutually recursive bindings
  | Lif Lexp Lexp Lexp -- Conditional
  | Ltuple [Lexp] -- Tuple construction
  | Lfetch Lexp [Var] Lexp -- Tuple destructuring
  deriving (Show, Eq)

---------------------------------------------------------------------------
-- Sexp to Lexp                                                          --
---------------------------------------------------------------------------

type Error = String

argsNumError :: Sexp -> String
argsNumError x = "Insufficient arguments for expression " ++ showSexp x

argsMatchError :: Sexp -> String
argsMatchError x = "Couldn't match expected arguments in: " ++ showSexp x

unrecExp :: Sexp -> String
unrecExp x = "Unrecognized Psil expression: " ++ showSexp x

unrecType :: Sexp -> String
unrecType x = "Unrecognized Psil type: " ++ showSexp x

-- Converts a Sexp list into a Haskell list of its elements.
sexp2list :: Sexp -> Either Error [Sexp]
sexp2list s = loop s []
  where
    loop (Scons hds tl) acc = loop hds (tl : acc)
    loop Snil acc = Right acc
    loop _ _ = Left ("Improper list: " ++ show s)

-- Builds a Lexp from a Sexp.
s2l :: Sexp -> Either Error Lexp
s2l (Snum n) = Right (Lnum n)
s2l (Ssym s) = Right (Lvar s)
s2l se@(Scons _ _) = do
  selist <- sexp2list se
  case selist of
    [Ssym "hastype", e, t] -> Lhastype <$> s2l e <*> s2t t
    (Ssym "call" : es)
      | length es < 2 -> Left (argsNumError se)
      | otherwise -> s2l' se selist
    (Ssym "fun" : es)
      | length es < 2 -> Left (argsNumError se)
      | otherwise -> s2l' se selist
    (Ssym "let" : es)
      | null es -> Left (argsNumError se)
      | otherwise -> Llet <$> s2d se (init es) <*> s2l (last es)
    [Ssym "if", e1, e2, e3] -> Lif <$> s2l e1 <*> s2l e2 <*> s2l e3
    (Ssym "tuple" : es) -> Ltuple <$> mapM s2l es
    [Ssym "fetch", tpl, xs, e] -> do
      tpl' <- s2l tpl
      names <- sexp2list xs >>= mapM asVar
      e' <- s2l e
      Right (Lfetch tpl' names e')
    _ -> Left (unrecExp se)
  where
    asVar x = case s2l x of
      Right (Lvar s) -> Right s
      _ -> Left (argsMatchError se)
s2l se = Left (unrecExp se)

-- Helper for s2l: handles currying for function definitions and calls.
s2l' :: Sexp -> [Sexp] -> Either Error Lexp
s2l' se selist =
  case selist of
    [Ssym "call", e, e1] -> Lcall <$> s2l e <*> s2l e1
    (Ssym "call" : es) ->
      Lcall <$> s2l' se (Ssym "call" : init es) <*> s2l (last es)
    [Ssym "fun", v, e] -> do
      v' <- s2l v
      case v' of
        Lvar x -> Lfun x <$> s2l e
        _ -> Left (argsMatchError se)
    (Ssym "fun" : v : vs) -> do
      v' <- s2l v
      case v' of
        Lvar x -> Lfun x <$> s2l' se (Ssym "fun" : vs)
        _ -> Left (argsMatchError se)
    _ -> Left (unrecExp se)

-- Builds an Ltype from a Sexp (used wherever a type is written).
s2t :: Sexp -> Either Error Ltype
s2t (Ssym "Int") = Right Lint
s2t (Ssym "Bool") = Right Lboo
s2t se@(Scons _ _) = do
  selist <- sexp2list se
  case selist of
    (Ssym "Tuple" : ts) -> Ltup <$> mapM s2t ts
    _
      | length selist < 2 -> Left (unrecType se)
      | last (init selist) == Ssym "->" -> s2t' se selist
      | otherwise -> Left (unrecType se)
s2t se = Left (unrecType se)

-- Helper for s2t: handles curried function types.
s2t' :: Sexp -> [Sexp] -> Either Error Ltype
s2t' se selist =
  case selist of
    [ta, Ssym "->", tr] -> Larw <$> s2t ta <*> s2t tr
    (t0 : rest@(_ : _))
      | last (init selist) == Ssym "->" -> Larw <$> s2t t0 <*> s2t' se rest
    _ -> Left (unrecType se)

-- Builds the (Var, Lexp) bindings of a let.
-- argNames and getTypes pull the argument names and the type out of a
-- function declaration for the type-checking and evaluation stages.
s2d :: Sexp -> [Sexp] -> Either Error [(Var, Lexp)]
s2d _ [] = Right []
s2d se (d : ds) = do
  selist <- sexp2list d
  if length selist < 2
    then Left ("Invalid declaration: " ++ showSexp se)
    else case selist of
      [Ssym x, e] -> bind x (s2l e)
      [Ssym x, t, e] -> bind x (Lhastype <$> s2l e <*> s2t t)
      (Ssym x : es) -> do
        args <- argNames (init (init es))
        typs <- getTypes (init es)
        bind
          x
          ( Lhastype
              <$> s2l' se (Ssym "fun" : args ++ [last es])
              <*> s2t' se typs
          )
      _ -> Left ("Unrecognized Psil declaration: " ++ showSexp se)
  where
    bind x body = do
      lexp <- body
      rest <- s2d se ds
      Right ((x, lexp) : rest)
    argNames = mapM firstSym
    firstSym a = do
      l <- sexp2list a
      case l of
        (v : _) -> Right v
        [] -> Left ("Empty argument: " ++ showSexp a)
    getTypes [] = Left "Type not specified"
    getTypes [t] = Right [Ssym "->", t]
    getTypes (t : ts) = do
      l <- sexp2list t
      rest <- getTypes ts
      case reverse l of
        (lastT : _) -> Right (lastT : rest)
        [] -> Left ("Empty type: " ++ showSexp t)
