module Folly.Resolution(
  isValid) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.Formula
import Folly.Theorem
import Folly.Unification

isValid :: (Show a) => Theorem a -> Bool
isValid t = not $ resolve $ deleteTautologies $ clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = uniqueVarNames $ L.concat $ L.map toClausalForm formulas
    clauseSet = S.fromList clauses

resolve :: (Show a) => Set [Formula a] -> Bool
resolve cls = case S.member [] cls of
  True -> True
  False -> resolveIter [cls]

resolveIter :: (Show a) => [Set [Formula a]] -> Bool
resolveIter [] = error "Empty list of clause sets"
resolveIter clauseSets = case S.size newClauses == 0 of
  True -> True
  False -> case S.member [] newClauses of
    True -> False
    False -> resolveIter (newClauses:clauseSets)
  where
    newClauses = case L.length clauseSets of
      1 -> generateNewClauses (head clauseSets) (head clauseSets)
      _ -> generateNewClauses (head clauseSets) (L.foldl S.union S.empty (tail clauseSets))

generateNewClauses :: (Show a) => Set [Formula a] -> Set [Formula a] -> Set [Formula a]
generateNewClauses recent old = deleteTautologies $ newClauses
  where
    newClauses = S.fold S.union S.empty $ S.map (\ c -> genNewClauses c old) recent
    genNewClauses c cs = S.fold S.union S.empty $ S.map (\ x -> resolvedClauses c x) cs

resolvedClauses :: (Show a) => [Formula a] -> [Formula a] -> Set [Formula a]
resolvedClauses left right = S.fromList resClauses
  where
    mResClauses = L.map (\ x -> (L.map (\ y -> tryToResolve x left y right) right)) left
    resClauses = L.map fromJust $ L.filter (/= Nothing) $ L.concat mResClauses 

tryToResolve :: (Show a) => Formula a -> [Formula a] -> Formula a -> [Formula a] -> Maybe [Formula a]
tryToResolve leftLiteral leftClause rightLiteral rightClause =
  case matchingLiterals leftLiteral rightLiteral of
    True -> unifiedResolvedClause leftLiteral leftClause rightLiteral rightClause
    False -> Nothing

unifiedResolvedClause :: (Show a) => Formula a -> [Formula a] -> Formula a -> [Formula a] -> Maybe [Formula a]
unifiedResolvedClause lLit lc rLit rc = case mostGeneralUnifier $ zip (literalArgs lLit) (literalArgs rLit) of
  Just mgu -> Just $ L.map (\ lit -> applyToTerms lit (applyUnifier mgu)) ((L.delete lLit lc) ++ (L.delete rLit rc))
  Nothing -> Nothing

uniqueVarNames :: [[Formula a]] -> [[Formula a]]
uniqueVarNames cls = zipWith attachSuffix cls (L.map show [1..length cls])

attachSuffix :: [Formula a] -> String -> [Formula a]
attachSuffix cls suffix = L.map (addSuffixToVarNames suffix) cls

addSuffixToVarNames :: String -> Formula a -> Formula a
addSuffixToVarNames suffix form = applyToTerms form (appendVarName suffix)

deleteTautologies :: Set [Formula a] -> Set [Formula a]
deleteTautologies clauses = S.filter (not . isTautology) clauses
