-- Psil: a small Lisp-like functional language.          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

module Main (main, run, sexpOf, lexpOf, typeOf, valOf) where

-- The interpreter is a single pipeline: lexer, parser, pretty printer,
-- lowering to a typed core, a type checker, and an evaluator.

import Control.Monad (foldM)
import Data.Char
import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec

---------------------------------------------------------------------------
-- Internal representation of expressions                                --
---------------------------------------------------------------------------
data Sexp
  = Snil -- Empty list
  | Scons Sexp Sexp -- Pair
  | Ssym String -- Symbol
  | Snum Int -- Integer
  deriving (Show, Eq)

-- Examples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Lexer                                                                 --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do _ <- char c; return ()

pComment :: Parser ()
pComment = do
  pChar ';'
  _ <- many (satisfy (/= '\n'))
  pChar '\n'
  return ()

pSpaces :: Parser ()
pSpaces =
  do _ <- many (do { _ <- space; return () } <|> pComment); return ()

integer :: Parser Int
integer =
  do
    c <- digit
    integer' (digitToInt c)
    <|> do
      _ <- satisfy (== '-')
      n <- integer
      return (-n)
  where
    integer' :: Int -> Parser Int
    integer' n =
      do
        c <- digit
        integer' (10 * n + digitToInt c)
        <|> return n

pSymchar :: Parser Char
pSymchar = alphaNum <|> satisfy (`elem` "!@$%^&*_+-=:|/?<>")

pSymbol :: Parser Sexp
pSymbol = do
  s <- many1 pSymchar
  return
    ( case parse integer "" s of
        Right n -> Snum n
        _ -> Ssym s
    )

---------------------------------------------------------------------------
-- Parser                                                                --
---------------------------------------------------------------------------

-- "'E" is shorthand for "(shorthand-quote E)", "`E" for
-- "(shorthand-backquote E)", and ",E" for "(shorthand-comma E)".
pQuote :: Parser Sexp
pQuote = do
  c <- satisfy (`elem` "'`,")
  pSpaces
  Scons
    ( Scons
        Snil
        ( Ssym
            ( case c of
                ',' -> "shorthand-comma"
                '`' -> "shorthand-backquote"
                _ -> "shorthand-quote"
            )
        )
    )
    <$> pSexp

-- A list (Tsil) has the form ( [e .] {e} ).
pTsil :: Parser Sexp
pTsil = do
  _ <- char '('
  pSpaces
  ( do _ <- char ')'; return Snil
      <|> do
        hd <-
          ( do
              e <- pSexp
              pSpaces
              ( do
                  _ <- char '.'
                  pSpaces
                  return e
                  <|> return (Scons Snil e)
                )
          )
        pLiat hd
    )
  where
    pLiat :: Sexp -> Parser Sexp
    pLiat hd =
      do
        _ <- char ')'
        return hd
        <|> do
          e <- pSexp
          pSpaces
          pLiat (Scons hd e)

-- Accepts any character; used to report errors.
pAny :: Parser (Maybe Char)
pAny = (Just <$> anyChar) <|> return Nothing

-- A Sexp is a list, a symbol, or an integer.
pSexpTop :: Parser Sexp
pSexpTop = do
  pTsil
    <|> pQuote
    <|> pSymbol
    <|> do
      x <- pAny
      case x of
        Nothing -> pzero
        Just c -> fail ("Unexpected char '" ++ [c] ++ "'")

-- A top-level Sexp and a sub-Sexp are parsed differently: a sub-Sexp failing
-- at EOF is a syntax error, whereas a top-level Sexp failing there is normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> fail "Unexpected end of stream"

-- A sequence of Sexps.
pSexps :: Parser [Sexp]
pSexps = do
  pSpaces
  many
    ( do
        e <- pSexpTop
        pSpaces
        return e
    )

-- Lets the parser back the generic "read".
instance Read Sexp where
  readsPrec _ s = case parse pSexp "" s of
    Left _ -> []
    Right e -> [(e, "")]

---------------------------------------------------------------------------
-- Sexp pretty printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = shows n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
  where
    showHead (Scons Snil e') = showString "(" . showSexp' e'
    showHead (Scons e1' e2') =
      showHead e1' . showString " " . showSexp' e2'
    showHead e = showString "(" . showSexp' e . showString " ."

-- Renders a Sexp back to its textual form.
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Core language                                                         --
---------------------------------------------------------------------------

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
-- getArgs and getTypes pull the argument names and the type out of a
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

---------------------------------------------------------------------------
-- Evaluator                                                             --
---------------------------------------------------------------------------

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

---------------------------------------------------------------------------
-- Type checker                                                          --
---------------------------------------------------------------------------

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

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> run path
    _ -> hPutStrLn stderr "usage: psil FILE.psil"

-- Reads a file of Sexps, evaluates each one, and prints its value and type.
-- A failing expression is reported and the rest still run.
run :: FilePath -> IO ()
run filename = do
  src <- readFile filename
  case parse pSexps filename src of
    Left err -> hPutStrLn stderr ("Parse error: " ++ show err)
    Right sexps -> mapM_ (putStrLn . evalTop) sexps
  where
    evalTop sexp = case s2l sexp >>= \lexp -> (,) lexp <$> infer tenv0 lexp of
      Left err -> "  error: " ++ err
      Right (lexp, ltyp) ->
        "  " ++ show (eval env0 lexp) ++ " : " ++ show ltyp

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Either Error Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Either Error Ltype
typeOf s = lexpOf s >>= infer tenv0

valOf :: String -> Either Error Value
valOf s = eval env0 <$> lexpOf s
