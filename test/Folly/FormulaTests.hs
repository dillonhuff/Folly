module Folly.FormulaTests(allFormulaTests) where

import Data.Set as S
import Folly.Formula
import Folly.TestUtils

allFormulaTests = do
  testFVT
  testVars
  
testFVT =
  testFunction fvt fvtCases
  
fvtCases =
  [(var "a", S.fromList [var "a"]),
   (func "+" [(var "x"), (var "y")], S.fromList [(var "x"), (var "y")]),
   (func "*" [(var "x"), (func "-" [(var "y"), (var "z")])],
    S.fromList [var "x", var "y", var "z"])]
  
testVars =
  testFunction vars varsCases
  
varsCases =
  [(t, S.empty),
   (f, S.empty),
   (pr "<" [(var "a"), (var "b")], S.fromList [(var "a"), (var "b")]),
   (imp (pr ">" [(var "c")]) (pr "<" [(var "d"), (var "e")]),
    S.fromList [(var "c"), (var "d"), (var "e")]),
   (neg (pr "isReal" [(var "i")]), S.fromList [(var "i")]),
   (te (var "x") (pr "=" [(var "k"), (var "j")]),
    S.fromList [(var "x"), (var "k"), (var "j")])]