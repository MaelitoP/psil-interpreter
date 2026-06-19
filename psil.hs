-- Psil: a small Lisp-like functional language.          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- The interpreter is a single pipeline: lexer, parser, pretty printer,
-- lowering to a typed core, a type checker, and an evaluator.

import Text.ParserCombinators.Parsec
import Data.Char
import System.Environment (getArgs)
import System.IO

---------------------------------------------------------------------------
-- Internal representation of expressions                                --
---------------------------------------------------------------------------
data Sexp = Snil                        -- Empty list
          | Scons Sexp Sexp             -- Pair
          | Ssym String                 -- Symbol
          | Snum Int                    -- Integer
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
pChar c = do { _ <- char c; return () }

pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (/= '\n'));
                pChar '\n'; return ()
              }

pSpaces :: Parser ()
pSpaces =
    do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (== '-')
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (`elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Parser                                                                --
---------------------------------------------------------------------------

-- "'E" is shorthand for "(shorthand-quote E)", "`E" for
-- "(shorthand-backquote E)", and ",E" for "(shorthand-comma E)".
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (`elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- A list (Tsil) has the form ( [e .] {e} ).
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepts any character; used to report errors.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- A Sexp is a list, a symbol, or an integer.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- A top-level Sexp and a sub-Sexp are parsed differently: a sub-Sexp failing
-- at EOF is a syntax error, whereas a top-level Sexp failing there is normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- A sequence of Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Lets the parser back the generic "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp pretty printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2') =
              showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- Convenience wrappers for reading and printing Sexps in GHCi.
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Core language                                                         --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint                   -- Int
           | Lboo                   -- Bool
           | Larw Ltype Ltype       -- τ₁ → τ₂
           | Ltup [Ltype]           -- tuple τ₁...τₙ
           deriving (Show, Eq)

data Lexp = Lnum Int                -- Integer constant
          | Lvar Var                -- Variable reference
          | Lhastype Lexp Ltype     -- Type annotation
          | Lcall Lexp Lexp         -- Function call (one argument)
          | Lfun Var Lexp           -- Anonymous one-argument function
          | Llet [(Var, Lexp)] Lexp -- Possibly mutually recursive bindings
          | Lif Lexp Lexp Lexp      -- Conditional
          | Ltuple [Lexp]           -- Tuple construction
          | Lfetch Lexp [Var] Lexp  -- Tuple destructuring
          deriving (Show, Eq)

---------------------------------------------------------------------------
-- Sexp to Lexp                                                          --
---------------------------------------------------------------------------

argsNumError :: Sexp -> String
argsNumError x = "Insufficient arguments for expression " ++ showSexp x

argsMatchError :: Sexp -> String
argsMatchError x = "Couldn't match expected arguments in: " ++ showSexp x

unrecExp :: Sexp -> String
unrecExp x = "Unrecognized Psil expression: " ++ showSexp x

unrecType :: Sexp -> String
unrecType x = "Unrecognized Psil type: " ++ showSexp x

-- Converts a Sexp list into a Haskell list of its elements.
sexp2list :: Sexp -> [Sexp]
sexp2list s = loop s []
    where
        loop (Scons hds tl) acc = loop hds (tl : acc)
        loop Snil acc = acc
        loop _ _ = error ("Improper list: " ++ show s)

-- Builds a Lexp from a Sexp.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (se@(Scons _ _)) =
    let
        selist = sexp2list se
    in
        case selist of
            [Ssym "hastype", e, t] -> Lhastype (s2l e) (s2t t)
            (Ssym "call" : es) ->
                if length es < 2
                then error (argsNumError se)
                else s2l' se selist
            (Ssym "fun" : es) ->
                if length es < 2
                then error (argsNumError se)
                else s2l' se selist
            (Ssym "let" : es) ->
                if null es
                then error (argsNumError se)
                else Llet (s2d se (init es)) (s2l (last es))
            [Ssym "if", e1, e2, e3] -> Lif (s2l e1) (s2l e2) (s2l e3)
            (Ssym "tuple" : es) -> Ltuple (map s2l es)
            [Ssym "fetch", tpl, xs, e] -> Lfetch (s2l tpl)
                (map (\x -> case s2l x of
                    Lvar s -> s
                    _ -> error (argsMatchError se))
                (sexp2list xs)) (s2l e)
            _ -> error (unrecExp se)
s2l se = error (unrecExp se)

-- Helper for s2l: handles currying for function definitions and calls.
s2l' :: Sexp -> [Sexp] -> Lexp
s2l' se selist =
    case selist of
        [Ssym "call", e, e1] -> Lcall (s2l e) (s2l e1)
        (Ssym "call" : es) ->
            Lcall (s2l' se (Ssym "call" : init es)) (s2l (last es))
        [Ssym "fun", v, e] ->
            case s2l v of
                Lvar x -> Lfun x (s2l e)
                _ -> error (argsMatchError se)
        (Ssym "fun" : v : vs) ->
            case s2l v of
                Lvar x -> Lfun x (s2l' se (Ssym "fun" : vs))
                _ -> error (argsMatchError se)
        _ -> error (unrecExp se)

-- Builds an Ltype from a Sexp (used wherever a type is written).
s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Ssym "Bool") = Lboo
s2t (se@(Scons _ _)) =
    let
        selist = sexp2list se
    in
        case selist of
            (Ssym "Tuple" : ts) -> Ltup (map s2t ts)
            _ | length selist < 2 -> error (unrecType se)
              | (last (init selist)) == Ssym "->" -> s2t' se selist
              | otherwise -> error (unrecType se)
s2t se = error (unrecType se)

-- Helper for s2t: handles curried function types.
s2t' :: Sexp -> [Sexp] -> Ltype
s2t' se selist =
    case selist of
        [ta, Ssym "->", tr] -> Larw (s2t ta) (s2t tr)
        _ | (last (init selist)) == Ssym "->" ->
              Larw (s2t (head selist)) (s2t' se (tail selist))
          | otherwise -> error (unrecType se)

-- Builds the (Var, Lexp) bindings of a let.
-- getArgs and getTypes pull the argument names and the type out of a
-- function declaration for the type-checking and evaluation stages.
s2d :: Sexp -> [Sexp] -> [(Var, Lexp)]
s2d _ [] = []
s2d se (d : ds) =
    let
        getArgs [] = []
        getArgs (a : as) = (head (sexp2list a)) : getArgs as
        getTypes [] = error "Type not specified"
        getTypes [t] = [Ssym "->", t]
        getTypes (t : ts) = (last (sexp2list t)) : getTypes ts
        selist = sexp2list d
    in
        if length selist < 2
        then error ("Invalid declaration: " ++ (showSexp se))
        else
            case selist of
                [Ssym x, e] -> (x, s2l e) : s2d se ds
                [Ssym x, t, e] -> (x, Lhastype (s2l e) (s2t t))
                    : s2d se ds
                (Ssym x : es) -> (x, Lhastype
                    (s2l' se (Ssym "fun" : getArgs (init (init es))
                        ++ [last es]))
                    (s2t' se (getTypes (init es)))) : (s2d se ds)
                _ -> error ("Unrecognized Psil declaration: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Evaluator                                                             --
---------------------------------------------------------------------------

-- Values produced by the evaluator.
data Value = Vnum Int
           | Vbool Bool
           | Vtuple [Value]
           | Vfun (Maybe String) (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec p (Vtuple vs) = showValues "[" vs
        where showValues _ [] = showString "]"
              showValues sep (v:vs')
                = showString sep . showsPrec p v . showValues " " vs'
    showsPrec _ (Vfun (Just n) _) =
          showString "<fun " . showString n . showString ">"
    showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- Initial environment: the built-in functions and their types.
env0 :: Env
env0 = [prim "+"  (+) Vnum  Lint,
        prim "-"  (-) Vnum  Lint,
        prim "*"  (*) Vnum  Lint,
        prim "/"  div Vnum  Lint,
        prim "="  (==) Vbool Lboo,
        prim ">=" (>=) Vbool Lboo,
        prim "<=" (<=) Vbool Lboo]
       where prim name op cons typ =
              (name,
               Vfun (Just name)
                    (\ (Vnum x) -> Vfun Nothing
                                       (\ (Vnum y) -> cons (x `op` y))),
               Larw Lint (Larw Lint typ))

-- Entry point for evaluation.
eval :: Env -> Lexp -> Value
eval env e =
  -- Split the env into names and values; types are unused at eval time.
  eval2 (map (\(x,_,_) -> x) env) e (map (\(_,v,_) -> v) env)

e2lookup :: [Var] -> Var -> Int          -- Find position within environment
e2lookup env x = e2lookup' env 0
    where e2lookup' :: [Var] -> Int -> Int
          e2lookup' [] _ = error ("Unknown variable: " ++ show x)
          e2lookup' (x':_) i | x == x' = i
          e2lookup' (_:xs) i = e2lookup' xs (i + 1)

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
    let
        i = e2lookup senv x
    in
        \venv -> venv !! i

eval2 senv (Lcall o a) = \venv ->
    let
        Vfun _ f = eval2' o
        n = eval2' a
        eval2' = \x -> (eval2 senv x) venv
    in
        f n

eval2 senv (Lfun a e) =
    \venv -> (Vfun Nothing (\v -> (eval2 (a : senv) e) (v : venv)))

eval2 senv (Llet ds b) = \venv ->
    let
        (vars, exps) = unzip ds
        senv' = vars ++ senv
        venv' = (map eval2' exps) ++ venv
        eval2' = \v -> (eval2 senv' v) venv'
    in
        eval2' b

eval2 senv (Lif e1 e2 e3) = \venv ->
    let
        eval2' = \x -> (eval2 senv x) venv
    in
        case eval2' e1 of
            Vbool True -> eval2' e2
            _ -> eval2' e3

eval2 senv (Ltuple e) = \venv ->
    let
        eval2' = \v -> (eval2 senv v) venv
    in
        Vtuple (map eval2' e)

eval2 senv (Lfetch tup vs e) = \venv ->
    let
        Vtuple tuplist = (eval2 senv tup) venv
        senv' = vs ++ senv
        venv' = tuplist ++ venv
    in
        (eval2 senv' e) venv'

---------------------------------------------------------------------------
-- Type checker                                                          --
---------------------------------------------------------------------------

type TEnv = [(Var, Ltype)]
type TypeError = String

-- Values are irrelevant to type checking, so keep only the types from env0.
tenv0 :: TEnv
tenv0 = (map (\(x,_,t) -> (x,t)) env0)

-- Looks up the type of a variable.
tlookup :: [(Var, a)] -> Var -> a
tlookup [] x = error ("Unknown variable: " ++ x)
tlookup ((x',t):_) x | x == x' = t
tlookup (_:env) x = tlookup env x

-- Typing rules: type synthesis.
infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar x) = tlookup tenv x

infer tenv (Lhastype e t)
    | te == Nothing = t
    | otherwise = let Just msg = te in error msg
    where
        te = check tenv e t

infer tenv (Lcall e1 e2)
    | te == Nothing = t2
    | otherwise = let Just msg = te in error msg
    where
        Larw t1 t2 = infer tenv e1
        te = check tenv e2 t1

infer tenv (Llet ds b) =
    let
        (tenvn, tenvt) = unzip tenv
        (vars, exps) = unzip ds
        tenvn' = vars ++ tenvn
        tenvt' = (map infer' exps) ++ tenvt
        infer' = \e -> infer (zip tenvn' tenvt') e
    in
        infer' b

infer tenv (Ltuple es) = Ltup (map (infer tenv) es)
infer _ (Lfun _ _)     = error "Can't infer type of `fun`"
infer _ (Lif _ _ _)    = error "Can't infer type of `if`"
infer _ (Lfetch _ _ _) = error "Can't infer type of `fetch`"

-- Typing rules: checking judgment.
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x body) (Larw t1 t2) = check ((x, t1) : tenv) body t2
check _ (Lfun _ _) t = Just ("Not a function type: " ++ show t)

check tenv (Lif e1 e2 e3) t
    | te1 /= Nothing = te1
    | te2 /= Nothing = te2
    | te3 /= Nothing = te3
    | otherwise = Nothing
    where
        te1 = check tenv e1 Lboo
        te2 = check tenv e2 t
        te3 = check tenv e3 t

check tenv (Lfetch tup xs e) t =
    let
        tupleType = case tup of
            Ltuple _ -> let Ltup list = infer tenv tup in list
            Lvar var -> case tlookup tenv var of
                Ltup list -> list
                _ -> error ("Not a tuple")
            _ -> error ("Not a tuple")
        tuplength = length tupleType
        varlength = length xs
    in
        if tuplength /= varlength
        then error ("Tuple length and number of variables mismatch: " ++
            show tuplength ++ " != "  ++ show varlength)
        else check ((zip xs tupleType) ++ tenv) e t

check tenv e t =
    -- Fall back to synthesis and compare with the expected type.
    let
        t' = infer tenv e
    in
        if t == t' then Nothing
        else Just ("Type mismatch: " ++ show t ++ " != " ++ show t')

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
run :: FilePath -> IO ()
run filename =
    do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in (concat
                (map (\ sexp -> let { ltyp = infer tenv0 lexp
                                   ; lexp = s2l sexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = infer tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
