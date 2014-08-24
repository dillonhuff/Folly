module Folly.Theorem(
  Theorem,
  theorem,
  hypothesis,
  conclusion) where

import Data.List as L

import Folly.Formula

data Theorem = Theorem [Formula] Formula
               deriving (Eq, Ord)

instance Show Theorem where
  show = showThm

showThm :: Theorem -> String
showThm (Theorem h c) = "Hypothesis:\n" ++ hypStr ++ "\n\n|=\n\n" ++ conclStr
  where
    hypStr = (L.concat $ L.intersperse "\n" $ L.map show h)
    conclStr = "Conclusion:\n" ++ show c

theorem = Theorem

hypothesis :: Theorem -> [Formula]
hypothesis (Theorem hp _) = hp

conclusion :: Theorem -> Formula
conclusion (Theorem _ c) = c
