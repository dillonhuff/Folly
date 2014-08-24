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
    (fa (var "a") (neg (pr "Kills" [constant "John", var "a"]))), True)]
