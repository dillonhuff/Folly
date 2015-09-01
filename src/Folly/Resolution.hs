module Folly.Resolution(isValid) where

import Data.List as L
import Data.Set as S

import Folly.Clause as C
import Folly.Formula
import Folly.Theorem

isValid :: Theorem -> Bool
isValid t = not $ resolve $ deleteTautologies $ clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = L.map (givenClause . S.fromList) $ uniqueVarNames $ toClausalForm $ L.foldr (\l r -> con l r) (head formulas) (tail formulas)
    clauseSet = S.fromList clauses

resolve :: Set Clause -> Bool
resolve cls = case S.member C.empty cls of
  True -> False
  False -> resolveIter [cls]

resolveIter :: [Set Clause] -> Bool
resolveIter [] = error "Empty list of clause sets"
resolveIter clauseSets = case S.size newClauses == 0 of
  True -> True
  False -> case S.member C.empty newClauses of
    True ->  error $ showTrace $ L.head $ L.filter (== C.empty) $ S.toList newClauses
    False -> resolveIter (newClauses:clauseSets)
  where
    newClauses =
      case L.length clauseSets of
       1 -> generateNewClauses (head clauseSets) (L.foldl S.union S.empty clauseSets)
       _ -> generateNewClauses (head clauseSets) (L.foldl S.union S.empty $ L.tail clauseSets)

generateNewClauses :: Set Clause -> Set Clause -> Set Clause
generateNewClauses recent old = deleteTautologies $ newClauses
  where
    newClauses = S.fold S.union S.empty $ S.map (\c -> genNewClauses c old) recent
    genNewClauses c cs = S.fold S.union S.empty $ S.map (\x -> resolvedClauses c x) cs

uniqueVarNames :: [[Formula]] -> [[Formula]]
uniqueVarNames cls = zipWith attachSuffix cls (L.map show [1..length cls])

attachSuffix :: [Formula] -> String -> [Formula]
attachSuffix cls suffix = L.map (addSuffixToVarNames suffix) cls

addSuffixToVarNames :: String -> Formula -> Formula
addSuffixToVarNames suffix form = applyToTerms form (appendVarName suffix)
