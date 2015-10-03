module Main(main) where

import System.Environment
import System.IO

import Folly.Lexer
import Folly.Parser
import Folly.Formula
import Folly.Resolution
import Folly.Theorem
import Folly.Utils

main :: IO ()
main = do
  (fileName:rest) <- getArgs
  fHandle <- openFile fileName ReadMode
  thmString <- hGetContents fHandle
  let thm = processTheoremFile thmString
  case thm of
    Left errMsg -> putStrLn errMsg
    Right t -> do
      putStr $ show t
      case isValid t of
        True -> putStrLn "\n\nIs Valid"
        False -> putStrLn "\n\nIs not valid"

processTheoremFile thmFileContents = (lexer thmFileContents) >>= parseTheorem
