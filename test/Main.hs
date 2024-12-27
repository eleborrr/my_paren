module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import      Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ arithTests, listTests]

arithTests = testGroup "Arith"
    [ testCase "2+2=4" $
        2+2 @?= (4 :: Int)
    , testCase "7 is odd" $
        assertBool "Oops, 7 is not odd" (odd ( 7 :: Int)) 
    ]

listTests = testGroup "List"
    [ testProperty "len (drop k xs) < len xs" $ property $ do
        xs <- forAll $ Gen.list (Range.linear 1 100) Gen.alpha
        k <- forAll $ Gen.int (Range.linear 1 10) 
        let n' = length (drop k xs)
            n = length xs
        diff n' (<) n
    ]
