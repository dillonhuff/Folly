module Folly.LexerTests() where

import Folly.Lexer
import Folly.TestUtils
import Folly.Utils

allLexerTests = do
  testFunction lexer lexerCases

lexerCases =
  [("nooo", Succeeded $ [testVar "nooo"]),
   ("(", Succeeded $ [testSep "("]),
   (")", Succeeded $ [testSep ")"]),
   ("[", Succeeded $ [testSep "["]),
   ("]", Succeeded $ [testSep "]"]),
   (",", Succeeded $ [testSep ","]),
   (".", Succeeded $ [testSep "."]),
   ("|", Succeeded $ [testOp "|"]),
   ("&", Succeeded $ [testOp "&"]),
   ("~", Succeeded $ [testOp "~"]),
   ("->", Succeeded $ [testOp "->"]),
   ("<->", Succeeded $ [testOp "<->"]),
   ("E", Succeeded $ [testQuant "E"]),
   ("Q", Succeeded $ [testQuant "Q"]),
   ("Ever", Succeeded $ [testPred "Ever"]),
   ("Quacks", Succeeded $ [testPred "Quacks"]),
   ("N#%_Man", Succeeded $ [testPred "N#%_Man"]),
   ("n#2@", Succeeded $ [testVar "n#2@"]),
   ("V z . E k. F[z] -> G[k]", Succeeded $ [testPred "V", testVar "z", testSep ".", testPred "E", testVar "k", testSep ".", testPred "F", testSep "[", testVar "z", testSep "]", testOp "->", testPred "G", testSep "[", testVar "k", testSep "]"])]
