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
    Failed errMsg -> putStrLn errMsg
    Succeeded t -> do
      putStr $ show t
      putStrLn $ "\n\nis " ++ (show $ isValid t)

processTheoremFile thmFileContents = (lexer thmFileContents) >>= parseTheorem
