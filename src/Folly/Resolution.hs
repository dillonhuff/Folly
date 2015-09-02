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
   False -> not $ resolve s $ clauses
  where
    clauses = p t

resolve :: (Set Clause -> Clause) -> Set Clause -> Bool
resolve s cls = case S.member C.empty cls of
  True -> False
  False -> resolve s (genNewClauses s cls)

genNewClauses s cls =
  let c = s cls
      newClauses = S.fold S.union S.empty $ S.map (\x -> resolvedClauses c x) (S.delete c cls) in
   S.union cls newClauses

{-resolveIter :: (Set Clause -> Clause) -> Set Clause -> Bool
resolveIter s clauseSet = case S.size clauseSet == 0 of
  True -> True
  False -> case S.member C.empty clauseSet of
    True -> False
    False -> resolveIter (newClauses:clauseSets)
  where
    newClauses = generateNewClauses (head clauseSets) (L.foldl S.union S.empty clauseSets)-}

generateNewClauses :: Set Clause -> Set Clause -> Set Clause
generateNewClauses recent old = deleteTautologies $ newClauses
  where
    newClauses = S.fold S.union S.empty $ S.map (\c -> genNewClauses c old) recent
    genNewClauses c cs = S.fold S.union S.empty $ S.map (\x -> resolvedClauses c x) cs
