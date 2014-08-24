module Folly.ResolutionTests(allResolutionTests) where

import Folly.Formula
import Folly.Resolution
import Folly.Theorem
import Folly.TestUtils

allResolutionTests = do
  testIsValid

testIsValid =
  testFunction isValid isValidTestCases

isValidTestCases =
  [(theorem [] (fa (var "a") (dis (pr "d" [var "a"]) (neg (pr "d" [var "a"])))), True),
   (theorem [] (pr "d" [var "k"]), False),
   (theorem [pr "Dog" [var "x"]] (pr "Dog" [var "x"]), True),
   (theorem
    [(fa (var "d") (imp (pr "Dog" [var "d"]) (pr "Loves" [constant "John", var "d"]))),
     (pr "Dog" [constant "Fido"])]
    (pr "Loves" [constant "John", constant "Fido"]), True),
   (theorem
    [(fa (var "p") (imp (te (var "d") (con (pr "Dog" [var "d"]) (pr "Owns" [var "p", var "d"]))) (pr "LovesAnimals" [var "p"]))),
     (imp (pr "LovesAnimals" [var "x"]) (fa (var "a") (neg (pr "Kills" [var "x", var "a"])))),
     (pr "Owns" [constant "John", constant "Fido"]),
     (pr "Dog" [constant "Fido"])]
    (fa (var "a") (neg (pr "Kills" [constant "John", var "a"]))), True),
   (theorem groupWithEquals (imp (con (con (con (pr "=" [func "*" [var "x", var "y"], constant "1"]) (pr "=" [func "*" [var "y", var "x"], constant "1"])) (pr "=" [func "*" [var "x", var "z"], constant "1"])) (pr "=" [func "*" [var "z", var "x"], constant "1"])) (pr "=" [var "y", var "z"])), True),
   (theorem groupWithEquals (imp (fa (var "x") (con (pr "=" [func "*" [var "x", var "z"], constant "1"]) (pr "=" [func "*" [var "z", var "x"], constant "1"]))) (pr "=" [var "z", constant "1"])), True)]

equalityAxioms =
  [(fa (var "x") (pr "=" [var "x", var "x"])),
   (fa (var "x") (fa (var "y") (imp (pr "=" [var "x", var "y"]) (pr "=" [var "y", var "x"])))),
   (fa (var "x") (fa (var "y") (fa (var "z") (imp (con (pr "=" [var "x", var "y"]) (pr "=" [var "y", var "z"])) (pr "=" [var "x", var "z"])))))]

groupWithEquals =
  [(fa (var "x") (te (var "y") (con (pr "=" [func "*" [var "x", var "y"], constant "1"]) (pr "=" [func "*" [var "y", var "x"], constant "1"])))),
   (fa (var "x") (fa (var "y") (fa (var "z") (pr "=" [func "*" [var "x", func "*" [var "y", var "z"]], func "*" [func "*" [var "x", var "y"], var "z"]])))),
   (fa (var "x") (con (pr "=" [func "*" [var "x", constant "1"], var "x"]) (pr "=" [func "*" [constant "1", var "x"], constant "x"])))] ++ equalityAxioms
