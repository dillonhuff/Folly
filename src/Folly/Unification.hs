module Folly.Unification(
  applyUnifier,
  unifier) where

import Data.List as L
import Data.Map as M
import Data.Set as S

import Folly.Formula

type Unifier = Map Term Term

unifier :: [(Term, Term)] -> Unifier
unifier subs = M.fromList subs

applyUnifier :: Unifier -> Term -> Term
applyUnifier u t = case possibleSubs u t of
  True -> applyUnifier u (subTerm u t)
  False -> t

possibleSubs :: Unifier -> Term -> Bool
possibleSubs u t = (length $ L.intersect (keys u) (S.toList (fvt t))) /= 0
