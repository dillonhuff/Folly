module Folly.ParserTests() where

import Folly.Formula
import Folly.Lexer
import Folly.Parser
import Folly.TestUtils
import Folly.Utils

allParserTests = do
  testFunction (\x -> ((>>=) x parseFormula)) (map (\ (x, y) -> (lexer x, y)) parseFormulaCases)

parseFormulaCases =
  [("Dog[ a ]", Succeeded $ pr "Dog" [var "a"]),
   ("~Super[ k, kill(a, b)]", Succeeded $ neg (pr "Super" [var "k", func "kill" [var "a", var "b"]])),
   ("Dog[x] & Owns[John, x]", Succeeded $ con (pr "Dog" [var "x"]) (pr "Owns" [constant "John", var "x"])),
   ("Ex[p] | ~Ex[x]", Succeeded $ dis (pr "Ex" [var "p"]) (neg (pr "Ex" [var "x"]))),
   ("Ex[p] -> ~Ex[x]", Succeeded $ imp (pr "Ex" [var "p"]) (neg (pr "Ex" [var "x"]))),
   ("Ex[p] <-> ~Ex[x]", Succeeded $ bic (pr "Ex" [var "p"]) (neg (pr "Ex" [var "x"])))]
