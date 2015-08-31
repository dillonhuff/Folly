module Folly.Resolution(isValid) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.DNF
import Folly.Formula
import Folly.Theorem
import Folly.Unification

isValid :: Theorem -> Bool
isValid t = not $ resolve $ deleteTautologies $ clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = L.map dnf $ uniqueVarNames $ L.concat $ L.map toClausalForm formulas
    clauseSet = S.fromList clauses

resolve :: Set [Formula] -> Bool
resolve cls = case S.member [] cls of
  True -> False
  False -> resolveIter [cls]

resolveIter :: [Set [Formula]] -> Bool
resolveIter [] = error "Empty list of clause sets"
resolveIter clauseSets = case S.size newClauses == 0 of
  True -> True
  False -> case S.member [] newClauses of
    True ->  False -- error $ "resolveIter: Clauses\n" ++ (show $ L.length $ S.toList newClauses) False
    False -> resolveIter (newClauses:clauseSets)
  where
    newClauses = case L.length clauseSets of
      1 -> generateNewClauses (head clauseSets) (head clauseSets)
      _ -> generateNewClauses (head clauseSets) (L.foldl S.union S.empty (tail clauseSets))

generateNewClauses :: Set [Formula] -> Set [Formula] -> Set [Formula]
generateNewClauses recent old = deleteTautologies $ newClauses
  where
    newClauses = S.fold S.union S.empty $ S.map (\c -> genNewClauses c old) recent
    genNewClauses c cs = S.fold S.union S.empty $ S.map (\x -> resolvedClauses c x) cs

resolvedClauses :: [Formula] -> [Formula] -> Set [Formula]
resolvedClauses left right = S.fromList resClauses
  where
    possibleResCombos = [(x, left, y, right) | x <- left, y <- right]
    mResClauses = L.map (\(x, l, y, r) -> tryToResolve x l y r) possibleResCombos
    resClauses = L.map fromJust $ L.filter (/= Nothing) mResClauses 

tryToResolve :: Formula -> [Formula] -> Formula -> [Formula] -> Maybe [Formula]
tryToResolve leftLiteral leftClause rightLiteral rightClause =
  case matchingLiterals leftLiteral rightLiteral of
    True -> unifiedResolvedClause leftLiteral leftClause rightLiteral rightClause
    False -> Nothing

unifiedResolvedClause :: Formula -> [Formula] -> Formula -> [Formula] -> Maybe [Formula]
unifiedResolvedClause lLit lc rLit rc =
  case mostGeneralUnifier $ zip (literalArgs lLit) (literalArgs rLit) of
   Just mgu ->
     Just $ L.map (\lit -> applyToTerms lit (applyUnifier mgu)) resolvedClause
   Nothing -> Nothing
  where
    resolvedClause = (L.delete lLit lc) ++ (L.delete rLit rc)

uniqueVarNames :: [[Formula]] -> [[Formula]]
uniqueVarNames cls = zipWith attachSuffix cls (L.map show [1..length cls])

attachSuffix :: [Formula] -> String -> [Formula]
attachSuffix cls suffix = L.map (addSuffixToVarNames suffix) cls

addSuffixToVarNames :: String -> Formula -> Formula
addSuffixToVarNames suffix form = applyToTerms form (appendVarName suffix)

deleteTautologies :: Set [Formula] -> Set [Formula]
deleteTautologies clauses = S.filter (not . isTautology) clauses

prettyClauseSets :: [Set [Formula]] -> String
prettyClauseSets css =
  L.concatMap prettyClauseSet css

prettyClauseSet cs =
  let formulas = S.toList cs in
   show formulas ++ "\n\n"
