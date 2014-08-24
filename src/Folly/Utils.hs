module Folly.Utils(
  Name,
  Error(..), extractValue) where

type Name = String

data Error a =
  Succeeded a |
  Failed String
  deriving (Show)

instance Monad Error where
  return a = Succeeded a
  (Succeeded a) >>= f = f a
  (Failed errMsg) >>= f = (Failed errMsg)

instance Eq a => Eq (Error a) where
  (==) (Succeeded v1) (Succeeded v2) = v1 == v2
  (==) _ _ = False

extractValue :: Error a -> a
extractValue (Succeeded val) = val
extractValue (Failed errMsg) = error $ "Computation Failed: " ++ errMsg
