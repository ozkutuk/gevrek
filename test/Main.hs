{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Core (Lit (..))
import Parser (parse', parseLit)
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [parserTests]

parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [literals]

literals :: TestTree
literals =
  testGroup
    "literals"
    [ bool
    , int
    ]

int :: TestTree
int =
  testGroup
    "int"
    [ testCase "positive" $ do
        parse' parseLit "42" @?= Right (LitInt 42)
        parse' parseLit "00042" @?= Right (LitInt 42)
    , expectFailBecause "not yet implemented" $
        testCase "negative" $
          parse' parseLit "-42" @?= Right (LitInt -42)
    , testCase "zero" $
        parse' parseLit "0" @?= Right (LitInt 0)
    ]

bool :: TestTree
bool = testCase "bool" $ do
  parse' parseLit "true" @?= Right (LitBool True)
  parse' parseLit "false" @?= Right (LitBool False)
