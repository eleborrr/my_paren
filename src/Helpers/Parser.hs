module Helpers.Parser where

import Helpers.Types
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L

expr :: Parser SExpr
expr = L.space space1 empty comment *> choice
    [ try nil
    , try ifExpr
    , try number
    , try bool
    , try stringLiteral
    , try symbol
    , try consCell
    , try arithOp
    , try compareOp
    , try quoteSugar
    , try quoteOp
    , atom
    ]

number :: Parser SExpr
number = Number <$> L.decimal

bool :: Parser SExpr
bool = Bool <$> ((char '#' *> (char 't' *> pure True <|> char 'f' *> pure False)))

stringLiteral :: Parser SExpr
stringLiteral = StringLiteral <$> (char '"' *> manyTill L.charLiteral (char '"'))

symbol :: Parser SExpr
symbol = Atom . (:[]) <$> (char '#' *> L.charLiteral)

consCell :: Parser SExpr
consCell = do
    _ <- char '('
    x <- expr
    _ <- space1 *> char '.' *> space1
    y <- expr
    _ <- char ')'
    return $ Cons x y

nil :: Parser SExpr
nil = string "()" *> pure Nil

atom :: Parser SExpr
atom = Atom <$> some (alphaNumChar <|> oneOf "!$%&*+-./:<=>?@^_~")

arithOp :: Parser SExpr
arithOp = do
    _ <- char '('
    op <- oneOf "+-/*"
    args <- many (space1 *> expr)
    _ <- char ')'
    return $ ArithOp op args

compareOp :: Parser SExpr
compareOp = do
    _ <- char '('
    op <- choice [string "=", string "<", string ">", string "<=", string ">="]
    args <- many (space1 *> expr)
    _ <- space
    _ <- char ')'
    return $ CompareOp op args

comment :: Parser ()
comment = L.skipLineComment ";"

scn :: Parser ()
scn = L.space space1 comment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) comment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

ifExpr :: Parser SExpr
ifExpr = do
    _ <- string "(if"
    cond <- space1 *> expr
    thenBranch <- space1 *> expr
    elseBranch <- space1 *> expr
    _ <- char ')'
    return $ If cond thenBranch elseBranch

quoteOp :: Parser SExpr
quoteOp = do
    _ <- string "(quote"
    space1
    exprValue <- expr
    _ <- char ')'
    return $ Quote exprValue

quoteSugar :: Parser SExpr
quoteSugar = do
    _ <- char '\''
    exprValue <- expr
    return $ Quote exprValue
