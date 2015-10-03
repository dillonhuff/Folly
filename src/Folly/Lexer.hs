module Folly.Lexer(
  Token, name, isVar, isPred, pos,
  testOp, testVar, testQuant,
  testPred, testSep,
  lexer) where

import Text.Parsec.Pos
import Text.ParserCombinators.Parsec
import Folly.Utils

data Token = 
  Var String SourcePos   |
  Pred String SourcePos  |
  Sep String SourcePos   |
  Op String SourcePos    |
  Quant String SourcePos |
  Res String SourcePos

instance Show Token where
  show = showTok
                      
instance Eq Token where
  (==) = tokEqual
  
isVar (Var _ _) = True
isVar _ = False

isPred (Pred _ _) = True
isPred _ = False

name (Var n _) = n
name (Pred n _) = n
name (Sep n _) = n
name (Op n _) = n
name (Res n _) = n
name (Quant n _) = n

pos (Var _ p) = p
pos (Pred _ p) = p
pos (Sep _ p) = p
pos (Op _ p) = p
pos (Res _ p) = p
pos (Quant _ p) = p

testVar s = Var s (newPos "DUMMY" 0 0)
testPred s = Pred s (newPos "DUMMY" 0 0)
testSep s = Sep s (newPos "DUMMY" 0 0)
testOp s = Sep s (newPos "DUMMY" 0 0)
testRes s = Res s (newPos "DUMMY" 0 0)
testQuant s = Quant s (newPos "DUMMY" 0 0)

showTok :: Token -> String
showTok t = name t

tokEqual :: Token -> Token -> Bool
tokEqual t1 t2 = name t1 == name t2

lexer :: String -> Error [Token]
lexer str = case parse parseToks "LEXER" str of
  Left err -> Left $ show err
  Right toks -> Right toks

parseToks = endBy parseTok spaces

parseTok = try atomicLit
         <|> try reservedWord
         <|> try predicate
         <|> try separator
         <|> try quantifier
         <|> operator

predicate = do
  pos <- getPosition
  firstChar <- upper <|> specialChar
  case firstChar of
    'E' -> eOrQPred 'E' pos
    'Q' -> eOrQPred 'Q' pos
    _ -> nonEQPred firstChar pos

eOrQPred firstChar pos = do
  rest <- many1 bodyChar
  return $ Pred (firstChar:rest) pos

nonEQPred firstChar pos = do
  rest <- many bodyChar
  return $ Pred (firstChar:rest) pos
  
atomicLit = do
  pos <- getPosition
  firstChar <- lower
  rest <- many bodyChar
  return $ Var (firstChar:rest) pos
  
reservedWord = do
  pos <- getPosition
  name <- try (string "HYPOTHESIS:") <|> (string "CONCLUSION:")
  return $ Res name pos

separator = do
  pos <- getPosition
  name <- choice $ map string ["(", ")", "]", "[", ",", "."]
  return $ Sep name pos

operator = do
  pos <- getPosition
  name <- choice $ map string ["~", "|", "&", "<->", "->"]
  return $ Op name pos

quantifier = do
  pos <- getPosition
  name <- choice $ map string ["E", "Q"]
  return $ Quant name pos

bodyChar = alphaNum <|> specialChar

specialChar = oneOf "!@#$%*<>?+=-_"
