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
  testToPNFPullQuants
  testSkolemize
  testToClausalForm
  
testFVT =
  testFunction fvt fvtCases

fvtCases :: [(Term, Set Term)]
fvtCases =
  [(var "a", S.fromList [var "a"]),
   (func "+" [(var "x"), (var "y")], S.fromList [(var "x"), (var "y")]),
   (func "*" [(var "x"), (func "-" [(var "y"), (var "z")])],
    S.fromList [var "x", var "y", var "z"]),
   (constant "1", S.empty)]
  
testVars =
  testFunction vars varsCases

varsCases :: [(Formula String, Set Term)]
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

freeVarCases :: [(Formula String, Set Term)]
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

generalizeCases :: [(Formula String, Formula String)]
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

testSub :: Map Term Term
testSub = M.fromList
          [(var "x", func "+" [var "a", var "b"]),
           (var "k", func "*" [var "c", var "d"])]

subTermCases :: [(Term, Term)]
subTermCases =
  [(constant "o", constant "o"),
   (var "x", func "+" [var "a", var "b"]),
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

subFormulaCases :: [(Formula String, Formula String)]
subFormulaCases =
  [(t, t),
   (f, f),
   (pr "a" [var "x"], pr "a" [var "a"]),
   (imp (pr "xo" [var "x"]) (pr "d" [var "y"]),
    imp (pr "xo" [var "a"]) (pr "d" [var "l"])),
   (neg (pr "isX" [var "x"]), neg (pr "isX" [var "a"])),
   (fa (var "q") (pr "isK" [var "x", constant "p"]),
    fa (var "q") (pr "isK" [var "a", constant "p"])),
   (te (var "a") (pr "gotcha" [var "x", var "a"]),
    te (var "a'") (pr "gotcha" [var "a", var "a'"])),
   (fa (var "a") (te (var "a'") (pr "=" [var "x", var "a'"])),
    fa (var "a'") (te (var "a''") (pr "=" [var "a", var "a''"])))]
  
testToPNF =
  testFunction toPNF pnfCases

pnfCases :: [(Formula String, Formula String)]
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
   (neg (neg (neg (pr "d" [var "o"]))), neg (pr "d" [var "o"])),
   (dis (neg (con (pr "Dog" [var "x"]) (pr "Owns" [var "y", var "x"]))) (pr "LovesAnimals" [var "y"]), (dis (dis (neg (pr "Dog" [var "x"])) (neg (pr "Owns" [var "y", var "x"]))) (pr "LovesAnimals" [var "y"])))]
  
testToPNFPullQuants =
  testFunction toPNF pullQuantsCases

pullQuantsCases :: [(Formula String, Formula String)]
pullQuantsCases =
  [(con (fa (var "x") (pr "d" [var "x"])) (fa (var "y") (pr "k" [var "y"])),
    fa (var "x") (con (pr "d" [var "x"]) (pr "k" [var "x"]))),
   (dis (te (var "k") (pr "d" [var "k"])) (te (var "j") (pr "i" [var "j"])),
    te (var "k") (dis (pr "d" [var "k"]) (pr "i" [var "k"]))),
   (dis (fa (var "k") (pr "d" [var "k"])) (pr "j" [var "k"]),
    fa (var "k'") (dis (pr "d" [var "k'"]) (pr "j" [var "k"]))),
   (dis (pr "d" [var "l"]) (fa (var "k") (pr "lj" [var "k"])),
    fa (var "k") (dis (pr "d" [var "l"]) (pr "lj" [var "k"]))),
   (dis (te (var "k") (pr "d" [var "k"])) (pr "j" [var "k"]),
    te (var "k'") (dis (pr "d" [var "k'"]) (pr "j" [var "k"]))),
   (dis (pr "d" [var "l"]) (te (var "k") (pr "lj" [var "k"])),
    te (var "k") (dis (pr "d" [var "l"]) (pr "lj" [var "k"]))),
   (con (fa (var "k") (pr "d" [var "k"])) (pr "j" [var "k"]),
    fa (var "k'") (con (pr "d" [var "k'"]) (pr "j" [var "k"]))),
   (con (pr "d" [var "l"]) (fa (var "k") (pr "lj" [var "k"])),
    fa (var "k") (con (pr "d" [var "l"]) (pr "lj" [var "k"]))),
   (con (te (var "k") (pr "d" [var "k"])) (pr "j" [var "k"]),
    te (var "k'") (con (pr "d" [var "k'"]) (pr "j" [var "k"]))),
   (con (pr "d" [var "l"]) (te (var "k") (pr "lj" [var "k"])),
    te (var "k") (con (pr "d" [var "l"]) (pr "lj" [var "k"]))),
   (con (con (fa (var "x") (pr "d" [var "x"])) (pr "i" [var "x"])) (pr "d" [var "k"]),
    fa (var "x'") (con (con (pr "d" [var "x'"]) (pr "i" [var "x"])) (pr "d" [var "k"])))]
  
testSkolemize =
  testFunction toSkolemForm skolemizeCases

skolemizeCases :: [(Formula String, Formula String)]
skolemizeCases =
  [(pr "d" [var "a"], pr "d" [var "a"]),
   (te (var "x") (pr "d" [var "x"]), pr "d" [skf 0 []]),
   (te (var "x") (fa (var "y") (pr "d" [var "x", var "y"])),
    fa (var "y") (pr "d" [skf 0 [], var "y"])),
   (fa (var "k") (te (var "l") (te (var "m") (pr "i" [var "l", var "m", var "k"]))),
    fa (var "k") (pr "i" [skf 0 [var "k"], skf 1 [var "k"], var "k"])),
   (fa (var "a") (te (var "b") (fa (var "c") (pr "l" [var "b", var "c", var "a"]))),
    fa (var "a") (fa (var "c") (pr "l" [skf 0 [var "a"], var "c", var "a"]))),
   (fa (var "a") (fa (var "b") (te (var "c") (pr "l" [var "a", var "c", var "b"]))),
    fa (var "a") (fa (var "b") (pr "l" [var "a", skf 0 [var "b", var "a"], var "b"])))]

testToClausalForm =
  testFunction toClausalForm clausalFormCases

clausalFormCases :: [(Formula String, [[Formula String]])]
clausalFormCases =
  [(pr "d" [var "no"], [[pr "d" [var "no"]]]),
   (fa (var "x") (con (pr "a" [var "x"]) (pr "d" [constant "P"])),
    [[pr "a" [var "x"]], [pr "d" [constant "P"]]]),
   (dis (pr "a" [var "x"]) (pr "d" [var "c"]), [[pr "a" [var "x"], pr "d" [var "c"]]]),
   (bic (pr "P" [var "x"]) (con (pr "Q" [var "y"]) (pr "R" [var "z"])),
    [[neg (pr "P" [var "x"]), pr "Q" [var "y"]],
     [neg (pr "P" [var "x"]), pr "R" [var "z"]],
     [neg (pr "Q" [var "y"]), neg (pr "R" [var "z"]), pr "P" [var "x"]]])]
