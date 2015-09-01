module TestMain(main) where

import Folly.FormulaTests
import Folly.LexerTests
import Folly.ParserTests
import Folly.ResolutionTests
import Folly.UnificationTests

main = do
  allFormulaTests
  allLexerTests
  allParserTests
  allResolutionTests
  allUnificationTests
