module Folly.Formula() where

import Data.List
import Proper.Sentence

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
  S String [Term] |
  P (Sentence Formula) |
  Q String Term String Formula
  deriving (Eq, Ord)
           
instance Show Formula where
  show = showFormula
  
showFormula :: Formula -> String
showFormula (S predName args) = predName ++ "(" ++ (concat $ intersperse ", " $ map showTerm args)  ++ ")"
showFormula (P sent) = "(" ++ show sent ++ ")"
showFormula (Q q t set f) = "(" ++ q ++ " "  ++ show t ++ " : " ++ set ++ " @ " ++ show f ++ ")"

te :: Term -> String -> Formula -> Formula
te v@(Var _) setType f = Q "E" v setType f
te t _ _ = error $ "Cannot quantify over non-variable term " ++ show t

fa :: Term -> String -> Formula -> Formula
fa v@(Var _) setType f = Q "V" v setType f
fa t _ _ = error $ "Cannot quantify over non-variable term " ++ show t