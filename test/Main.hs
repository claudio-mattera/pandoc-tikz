module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [
  ]
