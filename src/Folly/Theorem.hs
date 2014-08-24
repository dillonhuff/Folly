module Folly.Theorem(
  Theorem,
  theorem,
  hypothesis,
  conclusion) where

import Folly.Formula

data Theorem = Theorem [Formula] Formula
               deriving (Eq, Ord, Show)

theorem = Theorem

hypothesis :: Theorem -> [Formula]
hypothesis (Theorem hp _) = hp

conclusion :: Theorem -> Formula
conclusion (Theorem _ c) = c
