{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Control.Concurrent    (threadDelay)
import           Data.Text             (isSuffixOf)
import qualified Relude.Unsafe         (head)
import           System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute),
                                        OptDescr, getOpt)
import qualified System.Console.GetOpt as GetOpt (OptDescr (Option))
import           System.Environment    (getArgs)
import           System.FSNotify       (eventPath, watchTree, withManager)
import           System.IO             (hFlush)
import           TIE.Lib               (Response (..), Warnings, interoperate)

main :: IO ()
main = do
  args <- getArgs
  let (options, dirNames, errors) = getOpt Permute cliOptions args
  if not $ null errors then do
    forM_ errors putStr
    showUsage
    exitFailure
  else
    if Help `elem` options then
      showUsage >> exitSuccess
    else if length dirNames /= 1 then do
      putTextLn "Please provide just one directory containing Elm source."
      showUsage
      exitFailure
    else do
      let dirName = Relude.Unsafe.head dirNames
      interoperate dirName >>= printResponse
      if Watch `elem` options then do
        putTextLn "Watching for changes..."
        withManager \mgr -> do
          _ <- watchTree mgr dirName  (isSuffixOf ".elm" . toText . eventPath) .
            const $ do
              putTextLn "\nRegenerating..."
              interoperate dirName >>= printResponse
              hFlush stdout
          forever $ threadDelay 1000000
      else pass

data Flag = Help | Watch deriving (Eq, Show)

cliOptions :: [OptDescr Flag]
cliOptions =
  [ GetOpt.Option [] ["help"] (NoArg Help) "Print the program information and usage."
  , GetOpt.Option ['w'] ["watch"] (NoArg Watch) "Watch for and regenerate on changes."
  ]

showUsage :: IO ()
showUsage = do
  putTextLn "\nTIE - TypeScript Interoperator for Elm\n"
  putTextLn "Usage: tie [OPTIONS...] DIRNAME\n"
  putTextLn "Available options:"
  putTextLn "        --help    Print this help message"
  putTextLn "    -w  --watch   Watch for changes in the specified directory and regenerate"
  putTextLn "                  definitions every time a change occurs"
  putTextLn "        --colour  Print coloured output."
  putTextLn "        --color   Same as --colour.\n"

printResponse :: Response Text (FilePath, Warnings) -> IO ()
printResponse = \case
  Ok (path, warnings) -> do
    forM_  warnings \w -> putTextLn $ "Warning: " <> w
    putTextLn $ "Done! You can see the generated type definitions at " <> toText path
  Failed err -> do
    putTextLn "Some errors were encountered, so new TypeScript definitions were not generated:\n"
    forM_  (lines err) \l -> putTextLn $ "  - " <> l
