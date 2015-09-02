module Folly.Resolution(isValid) where

import Data.List as L
import Data.Set as S

import Folly.Clause as C
import Folly.Formula
import Folly.Theorem

isValid :: Theorem -> Bool
isValid t = isValid' maxClause standardSkolem t

maxClause cs = S.findMax cs

standardSkolem t = deleteTautologies $ clauseSet
  where
    formulas = (neg (conclusion t)) : (hypothesis t)
    clauses = L.map (givenClause . S.fromList) $ toClausalForm $ L.foldr (\l r -> con l r) (head formulas) (tail formulas)
    clauseSet = S.fromList clauses
  
isValid' :: (Set Clause -> Clause) -> -- Clause selection
            (Theorem -> Set Clause) -> -- Preprocessor
            Theorem -> Bool
isValid' s p t =
  case S.size clauses == 0 of
   True -> False
   False -> not $ resolve s (S.singleton (s clauses)) (S.delete (s clauses) clauses)
  where
    clauses = p t

resolve :: (Set Clause -> Clause) -> Set Clause -> Set Clause -> Bool
resolve s axioms cls = case S.member C.empty axioms ||
                            S.member C.empty cls of
  True -> False
  False -> case S.size cls == 0 of
    True -> True
    False ->
      let c = s cls
          newClauses = genNewClauses c axioms in
       resolve s (S.insert c axioms) (S.delete c $ S.union newClauses cls)

genNewClauses c cls =
  S.fold S.union S.empty $ S.map (\x -> S.union (resolvedClauses c x) (resolvedClauses x c)) (S.delete c cls)
