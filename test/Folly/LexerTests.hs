module Folly.LexerTests(allLexerTests) where

import Folly.Lexer
import Folly.TestUtils
import Folly.Utils

allLexerTests = do
  testFunction lexer lexerCases

lexerCases =
  [("nooo", Right $ [testVar "nooo"]),
   ("(", Right $ [testSep "("]),
   (")", Right $ [testSep ")"]),
   ("[", Right $ [testSep "["]),
   ("]", Right $ [testSep "]"]),
   (",", Right $ [testSep ","]),
   (".", Right $ [testSep "."]),
   ("|", Right $ [testOp "|"]),
   ("&", Right $ [testOp "&"]),
   ("~", Right $ [testOp "~"]),
   ("->", Right $ [testOp "->"]),
   ("<->", Right $ [testOp "<->"]),
   ("E", Right $ [testQuant "E"]),
   ("Q", Right $ [testQuant "Q"]),
   ("=", Right $ [testPred "="]),
   ("Ever", Right $ [testPred "Ever"]),
   ("Quacks", Right $ [testPred "Quacks"]),
   ("N#%_Man", Right $ [testPred "N#%_Man"]),
   ("n#2@", Right $ [testVar "n#2@"]),
   ("V z . E k. F[z] -> G[k]", Right $ [testPred "V", testVar "z", testSep ".", testPred "E", testVar "k", testSep ".", testPred "F", testSep "[", testVar "z", testSep "]", testOp "->", testPred "G", testSep "[", testVar "k", testSep "]"])]
