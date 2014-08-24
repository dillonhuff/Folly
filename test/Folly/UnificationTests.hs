module Folly.UnificationTests() where

import Folly.Formula
import Folly.TestUtils
import Folly.Unification

allUnificationTests = do
  testApplyUnifier
  testMostGeneralUnifier

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

testMostGeneralUnifier =
  testFunction mostGeneralUnifier mguTestCases

mguTestCases =
  [([(var "x", var "x")], Just $ unifier []),
   ([(var "x", var "y")], Just $ unifier [(var "x", var "y")]),
   ([(var "x", var "z"), (constant "p", constant "dsf")], Nothing),
   ([(func "g" [var "x2"], var "x1"),
     (func "f" [var "x1", func "h" [var "x1"], var "x2"],
      func "f" [func "g" [var "x3"], var "x4", var "x3"])],
    Just $ unifier [(var "x2", var "x3"),
                    (var "x1", func "g" [var "x3"]),
                    (var "x4", func "h" [func "g" [var "x3"]])])]
