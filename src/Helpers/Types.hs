module Helpers.Types where

import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void String

data SExpr = Atom String
        | Number Integer
        | StringLiteral String
        | Bool Bool
        | Cons SExpr SExpr
        | Nil
        | ArithOp Char [SExpr]
        | CompareOp String [SExpr]
        | If SExpr SExpr SExpr 
        | LogicBinary LogicOp SExpr SExpr
        | LogicUnary LogicOp SExpr
        | Quote SExpr
        | Define String SExpr
        | Lambda [String] SExpr
        deriving (Eq, Show, Read)

data LogicOp = And | Or | Not deriving (Show, Eq, Read)