module Folly.Clause(Clause,
                    clause, Folly.Clause.empty,
                    deleteTautologies,
                    resolvedClauses) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.Formula
import Folly.Unification

data Clause = Clause (Set Formula)
           deriving (Eq, Ord, Show)

clause = Clause

empty = Clause S.empty

deleteTautologies :: Set Clause -> Set Clause
deleteTautologies formulas = S.filter (not . dnfIsTautology) formulas

dnfIsTautology :: Clause -> Bool
dnfIsTautology (Clause lits) = isTautology lits

isTautology :: Set Formula -> Bool
isTautology c = S.size (S.intersection atoms negAtoms) > 0
  where
    atoms = S.filter isAtom c
    negAtoms = S.map stripNegations $ S.filter (not . isAtom) c

resolvedClauses :: Clause -> Clause -> Set Clause
resolvedClauses (Clause left) (Clause right) =
  case left == right of
   True ->  S.empty
   False -> S.fromList $ L.map (clause . S.fromList) $ resClauses
  where
    possibleResCombos = [(x, S.toList left, y, S.toList right) | x <- (S.toList left), y <- (S.toList right)]
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
