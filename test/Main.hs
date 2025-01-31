module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Helpers.Parser
import Helpers.Types
import MyLib

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , evalTests
  ]

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ testCase "Parse number" $
      runParser parseSExpr ["42"] @?= Right ([], Number 42)
  , testCase "Parse string literal" $
      runParser parseSExpr ["\"hello\""] @?= Right ([], StringLiteral "hello")
  , testCase "Parse boolean true" $
      runParser parseSExpr ["#t"] @?= Right ([], Bool True)
  , testCase "Parse boolean false" $
      runParser parseSExpr ["#f"] @?= Right ([], Bool False)
  , testCase "Parse list" $
      runParser parseSExpr ["(", "1", "2", "3", ")"] @?= Right ([], Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil)))
  , testCase "Parse arithmetic operation" $
      runParser parseSExpr ["(", "+", "1", "2", ")"] @?= Right ([], ArithOp '+' [Number 1, Number 2])
  , testCase "Parse define expression" $
      runParser parseSExpr ["(", "define", "x", "10", ")"] @?= Right ([], Define "x" (Number 10))
  , testCase "Parse lambda expression" $
      runParser parseSExpr ["(", "lambda", "(", "x", ")", "(", "+", "x", "1", ")", ")"] @?= Right ([], Lambda ["x"] (ArithOp '+' [Atom "x", Number 1]))
  ]

evalTests :: TestTree
evalTests = testGroup "Eval Tests"
  [ testCase "Eval number" $
      eval env (Number 42) @?= (Number 42, env)
  , testCase "Eval string literal" $
      eval env (StringLiteral "hello") @?= (StringLiteral "hello", env)
  , testCase "Eval boolean true" $
      eval env (Bool True) @?= (Bool True, env)
  , testCase "Eval boolean false" $
      eval env (Bool False) @?= (Bool False, env)
  , testCase "Eval arithmetic operation" $
      eval env (ArithOp '+' [Number 1, Number 2]) @?= (Number 3, env)
  , testCase "Eval variable" $
      eval env (Atom "x") @?= (Number 10, env)
  , testCase "Eval define expression" $
      let (_, newEnv) = eval env (Define "z" (Number 5))
      in eval newEnv (Atom "z") @?= (Number 5, newEnv)
  , testCase "Eval if expression (true branch)" $
      eval env (If (Bool True) (Number 1) (Number 2)) @?= (Number 1, env)
  , testCase "Eval if expression (false branch)" $
      eval env (If (Bool False) (Number 1) (Number 2)) @?= (Number 2, env)
  , testCase "Eval lambda expression" $
      eval env (Lambda ["x"] (ArithOp '+' [Atom "x", Number 1])) @?= (Lambda ["x"] (ArithOp '+' [Atom "x", Number 1]), env)
  ]

main :: IO ()
main = defaultMain tests