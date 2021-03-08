{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Control.Concurrent    (threadDelay)
import           Data.Text             (isSuffixOf)
import qualified Relude.Unsafe         (head)
import           System.Console.GetOpt (ArgOrder (Permute), getOpt)
import           System.Environment    (getArgs)
import           System.FSNotify       (eventPath, watchTree, withManager)
import           System.IO             (hFlush)
import           TIE.Lib               (Response (..), interoperate)

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
  else do
    let dirName = Relude.Unsafe.head dirNames
    interoperate dirName >>= printResponse
    withManager \mgr -> do
      _ <- watchTree mgr dirName  (isSuffixOf ".elm" . toText . eventPath) .
        const $ do
          putTextLn "\nRegenerating..."
          interoperate dirName >>= printResponse
          hFlush stdout
      forever $ threadDelay 1000000

printResponse :: Response Text FilePath -> IO ()
printResponse = \case
  Ok path -> putStrLn $ "Done! You can see the generated type definitions at " <> path
  Failed err -> do
    putTextLn err
    putTextLn "Regeneration failed. Check the logs above for more info."

showUsage :: IO ()
showUsage = putTextLn "Usage: tie DIRNAME"
