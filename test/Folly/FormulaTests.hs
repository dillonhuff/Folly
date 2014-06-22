module Folly.FormulaTests(allFormulaTests) where

import Data.Map as M
import Data.Set as S
import Folly.Formula
import Folly.TestUtils

allFormulaTests = do
  testFVT
  testVars
  testFreeVars
  testGeneralize
  testSubTerm
  testSubFormula
  
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
  
testFreeVars =
  testFunction freeVars freeVarCases
  
freeVarCases =
  [(t, S.empty),
   (f, S.empty),
   (pr "<" [(var "a"), (var "b")], S.fromList [(var "a"), (var "b")]),
   (imp (pr ">" [(var "c")]) (pr "<" [(var "d"), (var "e")]),
    S.fromList [(var "c"), (var "d"), (var "e")]),
   (neg (pr "isReal" [(var "i")]), S.fromList [(var "i")]),
   (te (var "x") (pr "=" [(var "x")]), S.empty)]

testGeneralize =
  testFunction generalize generalizeCases
  
generalizeCases =
  [(t, t),
   (f, f),
   (pr "!" [var "x"], (fa (var "x") (pr "!" [var "x"]))),
   (fa (var "y") (pr "is" [var "x", var "y"]),
    fa (var "x") (fa (var "y") (pr "is" [var "x", var "y"]))),
   (te (var "k") (pr "no" [var "l", var "k", var "s"]),
    (fa (var "s") (fa (var "l") (te (var "k") (pr "no" [var "l", var "k", var "s"])))))]
  
testSubTerm =
  testFunction (subTerm testSub) subTermCases
  
testSub = M.fromList
          [(var "x", func "+" [var "a", var "b"]),
           (var "k", func "*" [var "c", var "d"])]
          
subTermCases =
  [(var "x", func "+" [var "a", var "b"]),
   (func "su" [var "x", var "l"],
    func "su" [func "+" [var "a", var "b"], var "l"]),
   (func "op" [func "op" [var "k", var "x"]],
    func "op" [func "op"
               [func "*" [var "c", var "d"],
                func "+" [var "a", var "b"]]])]
  
testSubFormula =
  testFunction (subFormula testFormSub) subFormulaCases
  
testFormSub = M.fromList
              [(var "x", var "a"), (var "y", var "l")]
              
subFormulaCases =
  [(t, t),
   (f, f),
   (pr "a" [var "x"], pr "a" [var "a"]),
   (imp (pr "xo" [var "x"]) (pr "d" [var "y"]),
    imp (pr "xo" [var "a"]) (pr "d" [var "l"])),
   (neg (pr "isX" [var "x"]), neg (pr "isX" [var "a"])),
   (fa (var "q") (pr "isK" [var "x"]),
    fa (var "q") (pr "isK" [var "a"])),
   (te (var "a") (pr "gotcha" [var "x", var "a"]),
    te (var "a'") (pr "gotcha" [var "a", var "a'"])),
   (fa (var "a") (te (var "a'") (pr "=" [var "x", var "a'"])),
    fa (var "a'") (te (var "a''") (pr "=" [var "a", var "a''"])))]