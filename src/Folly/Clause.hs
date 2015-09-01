module Folly.Clause(Clause,
                    givenClause, Folly.Clause.empty,
                    deleteTautologies,
                    resolvedClauses,
                    showTrace) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.Formula
import Folly.Unification

data Clause = Clause (Set Formula) Justification
           deriving (Show)

instance Eq Clause where
  (==) (Clause cs1 _) (Clause cs2 _) = cs1 == cs2

instance Ord Clause where
  (<=) (Clause cs1 _) (Clause cs2 _) = cs1 <= cs2

givenClause cs = Clause cs Given
resolvedClause cs lc rc mgu = Clause cs (Resolved lc rc mgu)

empty = Clause S.empty Given

data Justification
  = Given
  | Resolved Clause Clause Unifier
    deriving (Eq, Ord, Show)

deleteTautologies :: Set Clause -> Set Clause
deleteTautologies formulas = S.filter (not . dnfIsTautology) formulas

dnfIsTautology :: Clause -> Bool
dnfIsTautology (Clause lits _) = isTautology lits

isTautology :: Set Formula -> Bool
isTautology c = S.size (S.intersection atoms negAtoms) > 0
  where
    atoms = S.filter isAtom c
    negAtoms = S.map stripNegations $ S.filter (not . isAtom) c

resolvedClauses :: Clause -> Clause -> Set Clause
resolvedClauses l@(Clause left _) r@(Clause right _) =
  case left == right of
   True ->  S.empty
   False -> S.fromList resClauses
  where
    possibleResCombos = [(x, l, y, r) | x <- (S.toList left), y <- (S.toList right)]
    mResClauses = L.map (\(x, l, y, r) -> tryToResolve x l y r) possibleResCombos
    resClauses = L.map fromJust $ L.filter (/= Nothing) mResClauses

tryToResolve :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
tryToResolve leftLiteral leftClause rightLiteral rightClause =
  case matchingLiterals leftLiteral rightLiteral of
    True -> unifiedResolvedClause leftLiteral leftClause rightLiteral rightClause
    False -> Nothing

unifiedResolvedClause :: Formula -> Clause -> Formula -> Clause -> Maybe Clause
unifiedResolvedClause lLit l@(Clause lc _) rLit r@(Clause rc _) =
  case L.length (literalArgs lLit) == L.length (literalArgs rLit) of
   True ->
     case mostGeneralUnifier $ zip (literalArgs lLit) (literalArgs rLit) of
      Just mgu ->
        Just $ mkResolvedClause mgu resolvedLits l r
      Nothing -> Nothing
   False -> Nothing
  where
    resolvedLits = S.union (S.delete lLit lc) (S.delete rLit rc)

mkResolvedClause :: Unifier -> Set Formula -> Clause -> Clause -> Clause
mkResolvedClause mgu lits lc rc =
  resolvedClause (S.map (\lit -> applyToTerms lit (applyUnifier mgu)) lits) lc rc mgu

showTrace c = showTraceRec 0 c

showTraceRec n c@(Clause cs Given) = (ind n) ++ "GIVEN " ++ show cs
showTraceRec n c@(Clause cs (Resolved a b u)) =
  (ind n) ++ show cs ++ "\t" ++ show u ++ "\n" ++
  showTraceRec (n+1) a ++ "\n" ++
  showTraceRec (n+1) b

ind n = L.concat $ L.replicate n "  "
