module Folly.ResolutionTests() where

import Folly.Formula
import Folly.Resolution
import Folly.Theorem
import Folly.TestUtils

allResolutionTests = do
  testIsValid

testIsValid =
  testFunction isValid isValidTestCases

isValidTestCases =
  [(theorem [] (fa (var "a") (dis (pr "d" [var "a"]) (neg (pr "d" [var "a"])))), True)]
