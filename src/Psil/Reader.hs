{-# OPTIONS_GHC -Wall #-}

-- Surface syntax: lexer, Parsec parser, and pretty-printer for s-expressions.
module Psil.Reader
  ( Sexp (..),
    pSexps,
    showSexp,
  )
where

import Data.Char
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
