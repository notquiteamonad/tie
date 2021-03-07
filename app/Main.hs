module Main where

import qualified Relude.Unsafe         (head)
import           System.Console.GetOpt (ArgOrder (Permute), getOpt)
import           System.Environment    (getArgs)
import           TIE.Lib               (interoperate)

main :: IO ()
main = do
  args <- getArgs
  let (_, dirNames, errors) = getOpt Permute [] args
  if not $ null errors then do
    forM_ errors putStr
    showUsage
    exitFailure
  else if length dirNames /= 1 then do
    putTextLn "Please provide just one directory containing Elm source."
    showUsage
    exitFailure
  else interoperate $ Relude.Unsafe.head dirNames

showUsage :: IO ()
showUsage = putTextLn "Usage: tie DIRNAME"
