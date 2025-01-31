module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Helpers.Parser
import Helpers.Types

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
      eval initialEnv (Number 42) @?= (Number 42, initialEnv)
  , testCase "Eval string literal" $
      eval initialEnv (StringLiteral "hello") @?= (StringLiteral "hello", initialEnv)
  , testCase "Eval boolean true" $
      eval initialEnv (Bool True) @?= (Bool True, initialEnv)
  , testCase "Eval boolean false" $
      eval initialEnv (Bool False) @?= (Bool False, initialEnv)
  , testCase "Eval arithmetic operation" $
      eval initialEnv (ArithOp '+' [Number 1, Number 2]) @?= (Number 3, initialEnv)
  , testCase "Eval variable" $
      eval initialEnv (Atom "x") @?= (Number 10, initialEnv)
  , testCase "Eval define expression" $
      let (_, newEnv) = eval initialEnv (Define "z" (Number 5))
      in eval newEnv (Atom "z") @?= (Number 5, newEnv)
  , testCase "Eval if expression (true branch)" $
      eval initialEnv (If (Bool True) (Number 1) (Number 2)) @?= (Number 1, initialEnv)
  , testCase "Eval if expression (false branch)" $
      eval initialEnv (If (Bool False) (Number 1) (Number 2)) @?= (Number 2, initialEnv)
  , testCase "Eval lambda expression" $
      eval initialEnv (Lambda ["x"] (ArithOp '+' [Atom "x", Number 1])) @?= (Lambda ["x"] (ArithOp '+' [Atom "x", Number 1]), initialEnv)
  ]

main :: IO ()
main = defaultMain tests