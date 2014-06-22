module Folly.Formula() where

import Data.List

data Term =
  Var String |
  Func String [Term]
  deriving (Eq, Ord)
           
instance Show Term where
  show = showTerm
  
showTerm :: Term -> String
showTerm (Var name) = name
showTerm (Func name args) = name ++ "(" ++ (concat $ intersperse ", " $ map showTerm args) ++ ")"

data Formula =
  P String [Term]              |
  B String Formula Formula     |
  N Formula                    |
  Q String Term String Formula
  deriving (Eq, Ord)
           
instance Show Formula where
  show = showFormula
  
showFormula :: Formula -> String
showFormula (P predName args) = predName ++ "(" ++ (concat $ intersperse ", " $ map showTerm args)  ++ ")"
showFormula (N f) = "~(" ++ show f ++ ")"
showFormula (B op f1 f2) = "(" ++ show f1 ++ " " ++ op ++ " "  ++ show f2 ++ ")"
showFormula (Q q t set f) = "(" ++ q ++ " "  ++ show t ++ " : " ++ set ++ " @ " ++ show f ++ ")"

te :: Term -> String -> Formula -> Formula
te v@(Var _) setType f = Q "E" v setType f
te t _ _ = error $ "Cannot quantify over non-variable term " ++ show t

fa :: Term -> String -> Formula -> Formula
fa v@(Var _) setType f = Q "V" v setType f
fa t _ _ = error $ "Cannot quantify over non-variable term " ++ show t

pr name args = P name args
con f1 f2 = B "&" f1 f2
dis f1 f2 = B "|" f1 f2
neg f = N f