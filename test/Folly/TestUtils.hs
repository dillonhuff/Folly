module Folly.TestUtils(testFunction) where

import Test.HUnit

testFunction func cases = runTestTT $ makeTestCases func cases

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ show input) expected (func input))