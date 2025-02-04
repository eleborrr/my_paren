module Helpers.Types where

data SExpr = Atom String
        | Number Integer
        | StringLiteral String
        | Bool Bool
        | Cons SExpr SExpr
        | Nil
        | ArithOp Char [SExpr]
        | CompareOp String [SExpr]
        | If SExpr SExpr SExpr 
        | LogicBinary LogicOp [SExpr]
        | LogicUnary LogicOp SExpr
        | Quote SExpr
        | Define String SExpr
        | Var String SExpr
        | Lambda [String] SExpr
        | StringCheck SExpr
        | StringLength SExpr
        | StringEqual SExpr SExpr
        | Substring SExpr SExpr SExpr
        | StringAppend [SExpr]  
        | Car SExpr
        | Cdr SExpr 
        deriving (Eq, Show, Read) 

data LogicOp = And | Or | Not deriving (Show, Eq, Read)


symbolCharacters :: String
symbolCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_!?-+*/%<>#'\"\\="

numberCharacters :: String
numberCharacters = "0123456789."

isSymbolCharacter :: Char -> Bool
isSymbolCharacter ch = elem ch symbolCharacters

isNumberCharacter :: Char -> Bool
isNumberCharacter ch = elem ch numberCharacters

isSymbol :: String -> Bool
isSymbol = all isSymbolCharacter

isNumber :: String -> Bool
isNumber = all isNumberCharacter