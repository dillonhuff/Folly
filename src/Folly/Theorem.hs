module Folly.Theorem(
  Theorem,
  theorem,
  hypothesis,
  conclusion) where

import Data.List as L

import Folly.Formula

data Theorem a = Theorem [Formula a] (Formula a)
               deriving (Eq, Ord)

instance (Show a) => Show (Theorem a) where
  show = showThm

showThm :: (Show a) => Theorem a -> String
showThm (Theorem h c) = "Hypothesis:\n" ++ hypStr ++ "\n\n|=\n\n" ++ conclStr
  where
    hypStr = (L.concat $ L.intersperse "\n" $ L.map show h)
    conclStr = "Conclusion:\n" ++ show c

theorem = Theorem

hypothesis :: Theorem a -> [Formula a]
hypothesis (Theorem hp _) = hp

conclusion :: Theorem a -> Formula a
conclusion (Theorem _ c) = c
