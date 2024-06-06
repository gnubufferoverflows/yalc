{-# LANGUAGE OverloadedStrings #-}

module Scanner where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type Label = [Char]
data Lambda = Var Label | Apply Lambda Lambda | Lam Label Lambda deriving Show

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` ["let", "true", "false", "and", "or", "not"]
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

var :: Parser Lambda
var = Var <$> identifier

lambda :: Parser Lambda
lambda = do
  symbol "\\"
  args <- some identifier
  symbol "."
  body <- expr
  return $ foldr Lam body args

apply :: Parser Lambda
apply = do
  expressions <- some term
  return $ foldl1 Apply expressions

term :: Parser Lambda
term = choice [var, lambda, between (symbol "(") (symbol ")") expr]

expr :: Parser Lambda
expr = apply <|> term

macro :: Parser (String, Lambda)
macro = do
  keyword "let"
  name <- identifier
  symbol "="
  body <- expr
  return (name, body)

macros :: Parser [(String, Lambda)]
macros = many macro

parseLambda :: Parser ([(String, Lambda)], Lambda)
parseLambda = do
  ms <- macros
  mainExpr <- expr
  return (ms, mainExpr)
