module Folly.Utils(
  Name,
  Error, extractValue) where

type Name = String

type Error a = Either String a

extractValue :: Error a -> a
extractValue (Right val) = val
extractValue (Left errMsg) = error $ "Computation Failed: " ++ errMsg
