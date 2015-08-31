module Folly.Formula(
  Term, Formula,
  fvt, subTerm, isVar, isConst, isFunc,
  funcName, funcArgs,
  appendVarName,
  var, func, constant,
  te, fa, pr, con, dis, neg, imp, bic, t, f,
  vars, freeVars, isAtom, stripNegations,
  generalize, subFormula,
  applyToTerms,
  literalArgs,
  toPNF, toSkolemForm, skf,
  toClausalForm,
  matchingLiterals) where

import Control.Monad
import Data.Set as S
import Data.List as L
import Data.Map as M

data Term =
  Constant String    |
  Var String         |
  Func String [Term]
  deriving (Eq, Ord)
           
instance Show Term where
  show = showTerm
   
showTerm :: Term -> String
showTerm (Constant name) = name
showTerm (Var name) = name
showTerm (Func name args) = name ++ "(" ++ (concat $ intersperse ", " $ L.map showTerm args) ++ ")"

isVar (Var _) = True
isVar _ = False

isFunc (Func _ _) = True
isFunc _ = False

isConst (Constant _) = True
isConst _ = False

funcName (Func n _) = n

funcArgs (Func _ a) = a

var n = Var n
func n args = case (L.take 3 n) == "skl" of
  True -> error $ "Function names beginning with skl are reserved for skolemization"
  False -> Func n args
constant n = Constant n

appendVarName :: String -> Term -> Term
appendVarName suffix (Var n) = Var (n ++ suffix)
appendVarName suffix (Func name args) = Func name $ L.map (appendVarName suffix) args
appendVarName _ t = t

fvt :: Term -> Set Term
fvt (Constant _) = S.empty
fvt (Var n) = S.fromList [(Var n)]
fvt (Func name args) = S.foldl S.union S.empty (S.fromList (L.map fvt args))

subTerm :: Map Term Term -> Term -> Term
subTerm _ (Constant name) = Constant name
subTerm sub (Func name args) = (Func name (L.map (subTerm sub) args))
subTerm sub (Var x) = case M.lookup (Var x) sub of
  Just s -> s
  Nothing -> (Var x)

data Formula =
  T                            | 
  F                            |
  P String [Term]              |
  B String Formula Formula     |
  N Formula                    |
  Q String Term Formula
  deriving (Eq, Ord)
           
instance Show Formula where
  show = showFormula
  
showFormula :: Formula -> String
showFormula T = "True"
showFormula F = "False"
showFormula (P predName args) = predName ++ "[" ++ (concat $ intersperse ", " $ L.map showTerm args)  ++ "]"
--showFormula (N (P name args)) = "~" ++ show (P name args)
showFormula (N f) = "~(" ++ show f ++ ")"
showFormula (B op f1 f2) = "(" ++ show f1 ++ " " ++ op ++ " "  ++ show f2 ++ ")"
showFormula (Q q t f) = "(" ++ q ++ " "  ++ show t ++ " . " ++ show f ++ ")"

applyToTerms :: Formula -> (Term -> Term) -> Formula
applyToTerms (P n args) f = P n $ L.map f args
applyToTerms (B n l r) f = B n (applyToTerms l f) (applyToTerms r f)
applyToTerms (Q n v l) f = Q n (f v) (applyToTerms l f)
applyToTerms (N l) f = N (applyToTerms l f)

te :: Term -> Formula -> Formula
te v@(Var _) f = Q "E" v f
te t _ = error $ "Cannot quantify over non-variable term " ++ show t

fa :: Term -> Formula -> Formula
fa v@(Var _) f = Q "V" v f
fa t _ = error $ "Cannot quantify over non-variable term " ++ show t

pr name args = P name args
con f1 f2 = B "&" f1 f2
dis f1 f2 = B "|" f1 f2
imp f1 f2 = B "->" f1 f2
bic f1 f2 = B "<->" f1 f2
neg f = N f
t = T
f = F

vars :: Formula -> Set Term
vars T = S.empty
vars F = S.empty
vars (P name terms) = S.fold S.union S.empty $ S.fromList (L.map fvt terms)
vars (B _ f1 f2) = S.union (vars f1) (vars f2)
vars (N f) = vars f
vars (Q _ v f) = S.insert v (vars f)

freeVars :: Formula -> Set Term
freeVars T = S.empty
freeVars F = S.empty
freeVars (P name terms) = S.fold S.union S.empty $ S.fromList (L.map fvt terms)
freeVars (B _ f1 f2) = S.union (freeVars f1) (freeVars f2)
freeVars (N f) = freeVars f
freeVars (Q _ v f) = S.delete v (freeVars f)

isAtom :: Formula -> Bool
isAtom (P _ _) = True
isAtom _ = False

stripNegations :: Formula -> Formula
stripNegations (N t) = t
stripNegations f = f

literalArgs :: Formula -> [Term]
literalArgs (P _ a) = a
literalArgs (N (P _ a)) = a
literalArgs l = error $ show l ++ " is not a literal"

matchingLiterals :: Formula -> Formula -> Bool
matchingLiterals (P n1 _) (N (P n2 _)) = n1 == n2
matchingLiterals (N (P n1 _)) (P n2 _) = n1 == n2
matchingLiterals (P _ _) (P _ _) = False
matchingLiterals (N (P _ _)) (N (P _ _)) = False
matchingLiterals l1 l2 = error $ show l1 ++ " or " ++ show l2 ++ " is not a literal"

generalize :: Formula -> Formula
generalize f = applyList genFreeVar f
  where
    genFreeVar = L.map fa (S.toList (freeVars f))

applyList :: [a -> a] -> a -> a
applyList [] a = a
applyList (f:fs) a = applyList fs (f a)

variant :: Set Term -> Term -> Term
variant vars x@(Var n) = case S.member x vars of
  True -> variant vars (Var (n ++ "'"))
  False -> x
  
subFormula :: Map Term Term -> Formula -> Formula
subFormula subst (P name args) = P name $ L.map (subTerm subst) args
subFormula subst (B op f1 f2) = B op (subFormula subst f1) (subFormula subst f2)
subFormula subst (N f) = N (subFormula subst f)
subFormula subst q@(Q _ _ _) = subQuant subst q
subFormula subst f = f

subQuant :: Map Term Term -> Formula -> Formula
subQuant subst (Q n v f) = case (M.filter (== v) subst) == M.empty of
  True -> Q n v (subFormula subst f)
  False -> Q n vNew $ subFormula (M.insert v vNew subst) f
  where
    vNew = variant (freeVars (subFormula (M.delete v subst) f)) v
    
    
toPNF :: Formula -> Formula
toPNF = (transformFormula pullQuantifiers) .
        (transformFormula simplifyFormula) .
        (transformFormula pushNegation) .
        (transformFormula elimVacuousQuantifiers) .
        (transformFormula replaceImp) .
        (transformFormula replaceBic)

pullQuantifiers f@(B "&" (Q "V" x p) (Q "V" y q)) = pullQ True True f fa con x y p q
pullQuantifiers f@(B "|" (Q "E" x p) (Q "E" y q)) = pullQ True True f te dis x y p q
pullQuantifiers f@(B "|" (Q "V" x p) q) = pullQ True False f fa dis x x p q
pullQuantifiers f@(B "|" p (Q "V" y q)) = pullQ False True f fa dis y y p q
pullQuantifiers f@(B "|" (Q "E" x p) q) = pullQ True False f te dis x x p q
pullQuantifiers f@(B "|" p (Q "E" y q)) = pullQ False True f te dis y y p q
pullQuantifiers f@(B "&" (Q "V" x p) q) = pullQ True False f fa con x x p q
pullQuantifiers f@(B "&" p (Q "V" y q)) = pullQ False True f fa con y y p q
pullQuantifiers f@(B "&" (Q "E" x p) q) = pullQ True False f te con x x p q
pullQuantifiers f@(B "&" p (Q "E" y q)) = pullQ False True f te con y y p q
pullQuantifiers f = f

pullQ :: Bool ->
         Bool ->
         Formula ->
         (Term -> Formula -> Formula) ->
         (Formula -> Formula -> Formula) ->
         Term ->
         Term ->
         Formula ->
         Formula ->
         Formula
pullQ l r f quant op x y p q =
  let z = variant (freeVars f) x in
  let ps = if l then subFormula (M.singleton x z) p else p in
  let qs = if r then subFormula (M.singleton y z) q else q in
  quant z (pullQuantifiers $ op ps qs)

simplifyFormula (N (N f)) = f
simplifyFormula (N T) = F
simplifyFormula (N F) = T
simplifyFormula (B "|" T f) = T
simplifyFormula (B "|" f T) = T
simplifyFormula (B "|" F F) = F
simplifyFormula (B "&" F f) = F
simplifyFormula (B "&" f F) = F
simplifyFormula (B "&" T T) = T
simplifyFormula f = f

pushNegation (N (B "|" f1 f2)) = B "&" (pushNegation (N f1)) (pushNegation (N f2))
pushNegation (N (B "&" f1 f2)) = B "|" (pushNegation (N f1)) (pushNegation (N f2))
pushNegation (N (Q "V" x f)) = Q "E" x (pushNegation (N f))
pushNegation (N (Q "E" x f)) = Q "V" x (pushNegation (N f))
pushNegation f = f

elimVacuousQuantifiers (Q n x f) = case S.member x (freeVars f) of
  True -> Q n x f
  False -> f
elimVacuousQuantifiers f = f

replaceImp (B "->" f1 f2) = dis (neg f1) f2
replaceImp f = f

replaceBic (B "<->" f1 f2) = con (imp f1 f2) (imp f2 f1)
replaceBic f = f

transformFormula :: (Formula -> Formula) -> Formula -> Formula
transformFormula tran (B op f1 f2) = tran (B op (transformFormula tran f1) (transformFormula tran f2))
transformFormula tran (Q q x f) = tran (Q q x (transformFormula tran f))
transformFormula tran (N f) = tran (N (transformFormula tran f))
transformFormula tran f = tran f

-- Conversion to Skolem form
toSkolemForm :: Formula -> Formula
toSkolemForm = skolemize . toPNF

skolemize :: Formula -> Formula
skolemize f = (transformFormula removeExistential) $ replaceVarsWithSkolemFuncs f

removeExistential :: Formula -> Formula
removeExistential (Q "E" v f) = f
removeExistential f = f

replaceVarsWithSkolemFuncs :: Formula -> Formula
replaceVarsWithSkolemFuncs f = subFormula varsToSkolemFuncs f
  where
    varsToSkolemFuncs = collectSkolemFuncs f 0 []
    
collectSkolemFuncs :: Formula -> Int -> [Term] -> Map Term Term
collectSkolemFuncs (Q "E" v f) n vars = M.insert v (skf n vars) (collectSkolemFuncs f (n+1) vars)
collectSkolemFuncs (Q "V" v f) n vars = collectSkolemFuncs f n (v:vars)
collectSkolemFuncs _ _ _ = M.empty

skf :: Int -> [Term] -> Term
skf n vars = Func ("skl" ++ show n) vars

-- Conversion to clausal form
toClausalForm :: Formula -> [[Formula]]
toClausalForm = splitClauses . removeUniversals . distributeDisjunction . toSkolemForm

distributeDisjunction :: (Formula) -> (Formula)
distributeDisjunction f = transformFormula distrDis f

distrDis :: (Formula) -> (Formula)
distrDis (B "|" (B "&" l r) f) = (B "&" (B "|" l f) (B "|" r f))
distrDis (B "|" f (B "&" l r)) = (B "&" (B "|" f l) (B "|" f r))
distrDis f = f

removeUniversals :: Formula -> Formula
removeUniversals (Q "V" v f) = removeUniversals f
removeUniversals f = f

splitClauses :: Formula -> [[Formula]]
splitClauses (B "&" l r) = (splitClauses l) ++ (splitClauses r)
splitClauses f = [splitDis f]

splitDis :: Formula -> [Formula]
splitDis (B "|" l r) = (splitDis l) ++ (splitDis r)
splitDis f = [f]
