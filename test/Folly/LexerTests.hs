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
   ("n#2@", Succeeded $ [testVar "n#2@"])]
