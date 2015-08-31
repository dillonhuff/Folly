module Folly.ResolutionTests(allResolutionTests) where

import Folly.Formula
import Folly.Resolution
import Folly.Theorem
import Folly.TestUtils

allResolutionTests = do
  testIsValid

testIsValid =
  testFunction isValid isValidTestCases

x = var "x"
y = var "y"
z = var "z"
a = var "a"
d = var "d"
p = var "p"

one = constant "1"
john = constant "John"
fido = constant "Fido"

times a b = func "*" [a, b]

eq a b = pr "=" [a, b]
dog d = pr "Dog" [d]
owns a b = pr "Owns" [a, b]
kills a b = pr "Kills" [a, b]
loves a b = pr "Loves" [a, b]
lovesAnimals a = pr "LovesAnimals" [a]

isValidTestCases :: [(Theorem, Bool)]
isValidTestCases =
  [(dog1, True),
   (dog2, False),
   (dog3, True),
   (dog4, True),
   (dog5, True),
   (dog6, False),
   (group1, True),
   (group2, True),
   (group3, False)]

dog1 = theorem [] (fa a (dis (dog a) (neg (dog a))))
dog2 = theorem [] (dog a)
dog3 = theorem [dog x] (dog x)
dog4 = theorem [(fa d (imp (dog d) (loves john d))), dog fido] (loves john fido)
dog5 = theorem
       [(fa p (imp (te d (con (dog d) (owns p d))) (lovesAnimals p))),
        (imp (lovesAnimals x) (fa a (neg (kills x a)))),
        owns john fido,
        dog fido]
       (fa a (neg (kills john a)))
dog6 = theorem [] (con (dog a) (neg $ dog a))

group1 = theorem
         groupAxioms
         (imp
          (con (con (con (eq (times x y) one) (eq (times y x) one)) (eq (times x z) one)) (eq (times z x) one))
          (eq y z))
group2 = theorem groupAxioms (imp (fa x (con (eq (times x z) one) (eq (times z x) one))) (eq z one))

group3 =
  theorem groupAxioms (fa x (fa y (eq (times x y) (times y x))))

equalityAxioms :: [Formula]
equalityAxioms =
   [(fa x (pr "=" [x, x])),
    (fa x (fa y (imp (eq x y) (eq y x)))),
    (fa x (fa y (fa z (imp (con (eq x y) (eq y z)) (eq x z)))))]

groupAxioms =
   [(fa x (te y (con (eq (times x y) one) (eq (times y x) one)))),
    (fa x (fa y (fa z (eq (times x (times y z)) (times (times x y) z))))),
    (fa x (con (eq (times x one) x) (eq (times one x) x)))] ++
   equalityAxioms
