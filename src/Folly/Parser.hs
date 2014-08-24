module Folly.Parser(
  parseFormula) where

import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim

import Folly.Formula
import Folly.Lexer as Lex
import Folly.Theorem
import Folly.Utils

parseTheoremToks toks = case parse parseTheorem "PARSER" toks of
  Left err -> Failed $ show err
  Right thm -> Succeeded thm

parseTheorem = do
  axioms <- parseAxioms
  hypothesis <- parseHypothesis
  return $ theorem axioms hypothesis
  
parseAxioms = do
  propTok "AXIOMS:"
  axioms <- many parseForm
  return axioms
  
parseHypothesis = do
  propTok "HYPOTHESIS:"
  hypothesis <- parseForm
  return hypothesis

parseFormula :: [Token] -> Error Formula
parseFormula toks = case parse parseForm "PARSER" toks of
  Left err -> Failed $ show err
  Right formula -> Succeeded formula

parseForm = buildExpressionParser table parseFactor

parseFactor = try (parseParens parseForm)
             <|> try parsePredicate
             <|> parseQuantification

table =
  [[negation],
   [conjunction],
   [disjunction],
   [implication],
   [bicondition]]

negation = Prefix parseNeg
conjunction = Infix parseCon AssocRight
disjunction = Infix parseDis AssocRight
implication = Infix parseImp AssocRight
bicondition = Infix parseBic AssocRight
--quantification = Prefix parseQuant

parseParens e = do
  propTok "("
  expr <- e
  propTok ")"
  return expr

parseQuantification = do
  quantType <- propTok "V" <|> propTok "E"
  varName <- varTok
  propTok "."
  form <- parseForm
  case (name quantType) of
    "V" -> return $ fa (var (name varName)) form
    "E" -> return $ te (var (name varName)) form
    _ -> error $ show quantType ++ " is not a quantifier"

parseNeg :: (Monad m) => ParsecT [Token] u m (Formula -> Formula)
parseNeg = do
  propTok "~"
  return $ neg
  
parseCon = do
  propTok "&"
  return $ con
  
parseDis = do
  propTok "|"
  return $ dis
  
parseImp = do
  propTok "->"
  return $ imp
  
parseBic = do
  propTok "<->"
  return $ bic

parsePredicate :: (Monad m) => ParsecT [Token] u m Formula
parsePredicate = do
  nameTok <- predicateTok
  propTok "["
  terms <- sepBy parseTerm (propTok ",")
  propTok "]"
  return $ pr (name nameTok) terms

parseTerm :: (Monad m) => ParsecT [Token] u m Term
parseTerm = try parseConstant <|> try parseFunc <|> parseVar

parseConstant :: (Monad m) => ParsecT [Token] u m Term
parseConstant = do
  nameTok <- predicateTok
  return $ constant (name nameTok)

parseVar :: (Monad m) => ParsecT [Token] u m Term
parseVar = do
  nameTok <- varTok
  return $ var (name nameTok)

parseFunc :: (Monad m) => ParsecT [Token] u m Term
parseFunc = do
  nameTok <- varTok
  propTok "("
  args <- sepBy parseTerm (propTok ",")
  propTok ")"
  return $ func (name nameTok) args

propTok :: (Monad m) => String -> ParsecT [Token] u m Token
propTok str = tokenPrim show updatePos hasNameStr
  where
    hasNameStr t = if (name t) == str then Just t else Nothing

predicateTok :: (Monad m) => ParsecT [Token] u m Token
predicateTok = tokenPrim show updatePos isPred
  where
    isPred t = if (Lex.isPred t) then Just t else Nothing

varTok :: (Monad m) => ParsecT [Token] u m Token
varTok = tokenPrim show updatePos isPred
  where
    isPred t = if (Lex.isVar t) then Just t else Nothing

literalTok :: (Monad m) => ParsecT [Token] u m Token
literalTok = tokenPrim show updatePos isLit
  where
    isLit t = if (Lex.isVar t) then Just t else Nothing

axiomsTok c = tokenPrim show updatePos isAxiom
  where
    isAxiom t = if (name t) == "AXIOMS:" then Just t else Nothing

hypothesisTok c = tokenPrim show updatePos isAxiom
  where
    isAxiom t = if (name t) == "HYPOTHESIS:" then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
