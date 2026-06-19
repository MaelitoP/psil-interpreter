{-# OPTIONS_GHC -Wall #-}

module Main (main, run, sexpOf, lexpOf, typeOf, valOf) where

import Psil.Core (Error, Lexp, Ltype, s2l)
import Psil.Eval (Value, env0, eval)
import Psil.Reader (Sexp, pSexps)
import Psil.Typecheck (infer, tenv0)
import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec (parse)

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
