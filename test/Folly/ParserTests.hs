module Folly.ParserTests(allParserTests) where

import Folly.Formula
import Folly.Lexer
import Folly.Parser
import Folly.TestUtils
import Folly.Utils

allParserTests = do
  testFunction (\x -> ((>>=) x parseFormula)) (map (\ (x, y) -> (lexer x, y)) parseFormulaCases)

parseFormulaCases :: [(String, Error Formula)]
parseFormulaCases =
  [("Dog[ a ]", Right $ pr "Dog" [var "a"]),
   ("~Super[ k, kill(a, b)]", Right $ neg (pr "Super" [var "k", func "kill" [var "a", var "b"]])),
   ("Dog[x] & Owns[John, x]", Right $ con (pr "Dog" [var "x"]) (pr "Owns" [constant "John", var "x"])),
   ("Dog [x] & (Owns[John, x] | Not[Cat, John])", Right $ con (pr "Dog" [var "x"]) (dis (pr "Owns" [constant "John", var "x"]) (pr "Not" [constant "Cat", constant "John"]))),
   ("Ex[p] | ~Ex[x]", Right $ dis (pr "Ex" [var "p"]) (neg (pr "Ex" [var "x"]))),
   ("Ex[p] -> ~Ex[x]", Right $ imp (pr "Ex" [var "p"]) (neg (pr "Ex" [var "x"]))),
   ("Ex[p] <-> ~Vx[x]", Right $ bic (pr "Ex" [var "p"]) (neg (pr "Vx" [var "x"]))),
   ("V x . U#12[x]", Right $ fa (var "x") (pr "U#12" [var "x"])),
   ("E y . K[f(l, y, No)]", Right $ te (var "y") (pr "K" [func "f" [var "l", var "y", constant "No"]])),
   ("V z . E k. F[z] -> G[k]", Right $ fa (var "z") (te (var "k") (imp (pr "F" [var "z"]) (pr "G" [var "k"])))),
   ("E y. K[y] -> V x. Z[x, y]", Right $ te (var "y") (imp (pr "K" [var "y"]) (fa (var "x") (pr "Z" [var "x", var "y"])))),
   ("E y. K[y] -> Z[x, y]", Right $ te (var "y") (imp (pr "K" [var "y"]) (pr "Z" [var "x", var "y"])))]
