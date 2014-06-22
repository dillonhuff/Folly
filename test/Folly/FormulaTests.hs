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
  testToPNF
  
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
  
testToPNF =
  testFunction toPNF pnfCases
  
pnfCases =
  [(t, t),
   (f, f),
   (imp (pr "a" [var "z"]) (pr "b" [var "z"]),
    dis (neg (pr "a" [var "z"])) (pr "b" [var "z"])),
   (bic (pr "d" [var "z"]) (pr "o" [var "k"]),
    con (dis (neg (pr "d" [var "z"])) (pr "o" [var "k"]))
        (dis (neg (pr "o" [var "k"])) (pr "d" [var "z"]))),
   (fa (var "x") (pr "d" [var "k", var "l"]),
    pr "d" [var "k", var "l"]),
   (fa (var "x") (te (var "d") (pr "i" [var "x", var "y"])),
    fa (var "x") (pr "i" [var "x", var "y"])),
   (neg (con (pr "a" [var "d"]) (pr "k" [var "l"])),
    dis (neg (pr "a" [var "d"])) (neg (pr "k" [var "l"]))),
   (neg (neg (dis (pr "a" [var "z"]) (pr "d" [var "k"]))),
    dis (pr "a" [var "z"]) (pr "d" [var "k"])),
   (neg (con (dis (pr "d" [var "x"]) (pr "e" [var "p"])) (pr "p" [var "w"])),
    dis (con (neg (pr "d" [var "x"])) (neg (pr "e" [var "p"]))) (neg (pr "p" [var "w"]))),
   (neg (fa (var "x") (pr "e" [var "x"])), te (var "x") (neg (pr "e" [var "x"]))),
   (neg (te (var "i") (pr "ds" [var "i"])), fa (var "i") (neg (pr "ds" [var "i"]))),
   (neg (con t f), t),
   (neg (neg (pr "d" [var "k"])), pr "d" [var "k"]),
   (dis f t, t),
   (dis f f, f),
   (con f t, f),
   (con t f, f),
   (con t t, t),
   (neg (neg (neg (pr "d" [var "o"]))), neg (pr "d" [var "o"]))]