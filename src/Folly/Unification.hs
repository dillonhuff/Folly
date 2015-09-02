module Folly.Unification(Unifier,
                         applyUnifier,
                         mostGeneralUnifier,
                         unifier,
                         uniqueVarSub) where

import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Set as S

import Folly.Formula

type Unifier = Map Term Term

uniqueVarSub :: [Term] -> [Term] -> Unifier
uniqueVarSub lt rt =
  let newVars = L.map (\v -> appendVarName "z" v) lt in
   case L.intersect newVars rt /= [] of
    True -> error $ "uniquVarSub: bad args " ++ show lt ++ " " ++ show rt
    False -> M.fromList $ L.zip lt newVars

unifier :: [(Term, Term)] -> Unifier
unifier subs = M.fromList subs

applyUnifier :: Unifier -> Term -> Term
applyUnifier u t = case possibleSubs u t of
  True -> applyUnifier u (subTerm u t)
  False -> t

possibleSubs :: Unifier -> Term -> Bool
possibleSubs u t = (length $ L.intersect (keys u) (S.toList (fvt t))) /= 0

mostGeneralUnifier :: [(Term, Term)] -> Maybe Unifier
mostGeneralUnifier toUnify = liftM M.fromList $ martelliMontanari toUnify

martelliMontanari :: [(Term, Term)] -> Maybe [(Term, Term)]
martelliMontanari [] = Just []
martelliMontanari ((t1, t2):rest) = case t1 == t2 of
  True -> martelliMontanari rest
  False -> case isVar t1 of
    True -> eliminateVar t1 t2 rest 
    False -> case isVar t2 of
      True -> martelliMontanari ((t2, t1):rest)
      False -> case isFunc t1 && isFunc t2 && funcName t1 == funcName t2 of
        True -> martelliMontanari $ (zip (funcArgs t1) (funcArgs t2)) ++ rest
        False -> Nothing

eliminateVar :: Term -> Term -> [(Term, Term)] -> Maybe [(Term, Term)]
eliminateVar var term rest = case S.member var (fvt term) of
  True -> Nothing
  False -> liftM ((:) (var, term)) $ martelliMontanari $ applyToAll (var, term) rest

applyToAll :: (Term, Term) -> [(Term, Term)] -> [(Term, Term)]
applyToAll sub toUnify = L.map applyToPair toUnify
  where
    applyToPair (x, y) = (applyUnifier uni x, applyUnifier uni y)
    uni = unifier [sub]
