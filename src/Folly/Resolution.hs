module Folly.Resolution(
  isValid) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.Formula
import Folly.Theorem
import Folly.Unification

isValid :: Theorem -> Bool
isValid t = not $ resolve clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = uniqueVarNames $ L.concat $ L.map toClausalForm formulas
    clauseSet = S.fromList clauses

resolve :: Set Clause -> Bool
resolve cls = case S.member [] cls of
  True -> False
  False -> resolveIter [cls]

resolveIter :: [Set Clause] -> Bool
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

generateNewClauses :: Set Clause -> Set Clause -> Set Clause
generateNewClauses recent old = newClauses
  where
    newClauses = S.fold S.union S.empty $ S.map (\ c -> genNewClauses c old) recent
    genNewClauses c cs = S.fold S.union S.empty $ S.map (\ x -> resolvedClauses c x) cs

resolvedClauses :: Clause -> Clause -> Set Clause
resolvedClauses left right = S.fromList resClauses
  where
    mResClauses = L.map (\ x -> (L.map (\ y -> tryToResolve x left y right) right)) left
    resClauses = L.map fromJust $ L.filter (/= Nothing) $ L.concat mResClauses 

tryToResolve :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
tryToResolve leftLiteral leftClause rightLiteral rightClause =
  case matchingLiterals leftLiteral rightLiteral of
    True -> unifiedResolvedClause leftLiteral leftClause rightLiteral rightClause
    False -> Nothing

unifiedResolvedClause :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
unifiedResolvedClause lLit lc rLit rc = case mostGeneralUnifier $ zip (literalArgs lLit) (literalArgs rLit) of
  Just mgu -> Just $ L.map (\ lit -> applyToTerms lit (applyUnifier mgu)) ((L.delete lLit lc) ++ (L.delete rLit rc))
  Nothing -> Nothing

uniqueVarNames :: [Clause] -> [Clause]
uniqueVarNames cls = cls
