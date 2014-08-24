module Folly.UnificationTests() where

import Folly.Formula
import Folly.TestUtils
import Folly.Unification

allUnificationTests = do
  testApplyUnifier

simpleUnifier = unifier [(var "d", constant "Fido"),
                         (var "n", constant "OOH-KILLEM"),
                         (var "k", func "x" [(var "y")]),
                         (var "y", var "n"),
                         (var "q", var "uio")]

testApplyUnifier =
  testFunction (applyUnifier simpleUnifier) applyUnifierCases

applyUnifierCases =
  [(var "should-not-change", var "should-not-change"),
   (var "q", var "uio"),
   (var "d", constant "Fido"),
   (var "k", func "x" [constant "OOH-KILLEM"]),
   (func "nest" [var "k"], func "nest" [func "x" [constant "OOH-KILLEM"]])]
