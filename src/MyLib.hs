module MyLib where

import Data.Void (Void)
import Text.Megaparsec
    ( oneOf,
      parse,
      errorBundlePretty,
      choice,
      many,
      manyTill,
      some,
      empty,
      (<|>),
      Parsec,
      MonadParsec(takeWhile1P, try) )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Helpers.Parser
import Helpers.Types


evaluate :: SExpr -> SExpr 

evaluate (Number n) = Number n

-- evaluate Nil = Nil

evaluate (Bool b) = Bool b

evaluate (Lambda args body) = Quote (Lambda args body)

evaluate (Quote exprValue) = exprValue 

evaluate (Define var value) = Quote value

evaluate (If cond thenBranch elseBranch) =
    case evaluate cond of
        Bool True -> evaluate thenBranch
        Bool False -> evaluate elseBranch
        _ -> error "Condition must evaluate to boolean"

evaluate Nil = Bool False    -- Если это Nil, возвращаем False (или любое другое значение по умолчанию)

-- Main evaluate function
evaluate (LogicBinary op firstBranch secondBranch) =
    case op of
        And -> Bool (exprToBool evaluatedFirst && exprToBool evaluatedSecond)
        Or  -> Bool (exprToBool evaluatedFirst || exprToBool evaluatedSecond)
        _ -> error "Unsupported logic operation"
  where 
    evaluatedFirst = evaluate firstBranch  -- Recursive call for the first operand
    evaluatedSecond = evaluate secondBranch

evaluate (LogicUnary op body) =
    case op of
        Not -> Bool (not (exprToBool evaluatedBody))  -- Handle Not with only one operand
        _ -> error "Unsupported logic operation"
  where 
    evaluatedBody = evaluate body  -- Recursive call for the first operand

-- evaluate (Logic op firstBranch ) =
--     case op of
--         Not -> case evaluatedFirst of
--                     (Bool b) -> Bool (not b)
--                     _ -> Bool False
--         _ -> error "Unsupported logic operation"
--     where 
--         evaluatedFirst = evaluate firstBranch

evaluate (ArithOp op args) = case op of
    '+' -> Number . sum $ map evaluateNumber args
    '-' -> Number . foldl1 (-) $ map evaluateNumber args
    '*' -> Number . product $ map evaluateNumber args
    '/' -> Number . foldl1 div $ map evaluateNumber args
    _   -> error "Unsupported operation"

evaluate (CompareOp op args) = case op of
    "="  -> if allEqual (map evaluateNumber args) then Bool True else Bool False
    "<"  -> if inOrder (<) (map evaluateNumber args) then Bool True else Bool False
    ">"  -> if inOrder (>) (map evaluateNumber args) then Bool True else Bool False
    "<=" -> if inOrder (<=) (map evaluateNumber args) then Bool True else Bool False
    ">=" -> if inOrder (>=) (map evaluateNumber args) then Bool True else Bool False
    _    -> error "Unsupported comparison operation"
  where
    allEqual xs = all (== head xs) (tail xs)
    inOrder f (x:y:ys) = f x y && inOrder f (y:ys)
    inOrder _ _ = True
evaluate _ = error "Unsupported expression type for evaluation"

exprToBool :: SExpr -> Bool
exprToBool (Bool False) = False
exprToBool _            = True

evaluateNumber :: SExpr -> Integer
evaluateNumber (Number n) = n
evaluateNumber _ = error "Expected a number"

test :: IO ()
test = do
    let filename = "test.txt"
    content <- readFile filename  
    let linesOfContent = lines content   
    mapM_ processLine linesOfContent

processLine :: String -> IO ()
processLine line = do
    print (tokenize line)
    case parse expr "" line of
        Left bundle -> putStrLn $ errorBundlePretty bundle 
        Right result -> print (evaluate result) 