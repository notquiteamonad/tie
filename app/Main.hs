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
import           TIE.Lib               (Response (..), interoperate)
import           Text.Colour           (bold, chunk, fore, green, putChunks,
                                        red, underline)

main :: IO ()
main = do
  args <- getArgs
  let (options, dirNames, errors) = getOpt Permute cliOptions args
  if not $ null errors then do
    forM_ errors putStr
    showUsage
    exitFailure
  else if Help `elem` options then
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
showUsage = putChunks
    [ bold $ chunk "\nTIE - TypeScript Interoperator for Elm\n\n"
    , bold $ chunk "Usage: "
    , fore green . bold $ chunk "tie "
    , bold $ chunk "[OPTIONS...] DIRNAME\n\n"
    , bold $ chunk "Available options:\n"
    , bold $ chunk "        --help   "
    , chunk "Print this help message\n"
    , bold $ chunk "    -w  --watch  "
    , chunk "Watch for changes in the specified directory and regenerate\n"
    , chunk "                 definitions every time a change occurs\n\n"
    ]

printResponse :: Response Text FilePath -> IO ()
printResponse = \case
  Ok path ->
    putChunks
      [ fore green . bold $ chunk "Done! You can see the generated type definitions at "
      , fore green . underline . chunk $ toText path <> "\n"
      ]
  Failed err -> do
    putChunks
      ((fore red . bold $ chunk "Some errors were encountered, so new TypeScript definitions were not generated:\n\n")
      : ((\l -> fore red . chunk $ "  - " <> l <> "\n") <$> lines err))
