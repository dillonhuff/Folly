module Folly.Resolution(isValid,
                        isValid',
                        standardSkolem,
                        maxClause) where

import Data.List as L
import Data.Maybe
import Data.Set as S

import Folly.Clause as C
import Folly.Formula
import Folly.Theorem

isValid :: Theorem -> Bool
isValid t =
  case isValid' maxClause standardSkolem t of
   Just _ -> True
   Nothing -> False

maxClause cs = S.findMax cs

standardSkolem t = deleteTautologies $ clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = L.map (givenClause . S.fromList) $ toClausalForm $ L.foldr (\l r -> con l r) (head formulas) (tail formulas)
    clauseSet = S.fromList clauses
  
isValid' :: (Set Clause -> Clause) -> -- Clause selection
            (Theorem -> Set Clause) -> -- Preprocessor
            Theorem -> Maybe Clause
isValid' s p t =
  case S.size clauses == 0 of
   True -> Nothing
   False ->
     case S.member C.empty clauses of
      True -> S.lookupGE C.empty clauses
      False -> resolve s (S.singleton (s clauses)) (S.delete (s clauses) clauses)
  where
    clauses = p t

resolve :: (Set Clause -> Clause) -> Set Clause -> Set Clause -> Maybe Clause
resolve s axioms cls =
  case S.member C.empty cls of
   True -> S.lookupGE C.empty cls
   False -> case S.size cls == 0 of
     True -> Nothing
     False ->
       let c = s cls
           newClauses = genNewClauses c axioms
           nextAxioms = S.insert c axioms
           nextCls = S.delete c $ S.union newClauses cls in
        resolve s nextAxioms nextCls

genNewClauses c cls =
  S.fold S.union S.empty $ S.map (\x -> S.union (resolvedClauses c x) (resolvedClauses x c)) (S.delete c cls)
