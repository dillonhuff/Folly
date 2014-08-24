module Folly.Resolution(
  isValid) where

import Data.List as L
import Data.Set as S

import Folly.Formula
import Folly.Theorem

isValid :: Theorem -> Bool
isValid t = not $ resolve clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = uniqueVarNames $ L.concat $ L.map toClausalForm formulas
    clauseSet = S.fromList clauses

resolve :: Set Clause -> Bool
resolve cls = case S.member [] cls of
  True -> False
  False -> True

uniqueVarNames :: [Clause] -> [Clause]
uniqueVarNames cls = cls
