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
import           Text.Colour           (Chunk,
                                        TerminalCapabilities (With8Colours, WithoutColours),
                                        bold, chunk, fore, green, putChunksWith,
                                        red, underline, yellow)

main :: IO ()
main = do
  args <- getArgs
  let (options, dirNames, errors) = getOpt Permute cliOptions args
  if not $ null errors then do
    forM_ errors putStr
    showUsage WithoutColours
    exitFailure
  else
    let tc = if Colour `elem` options then With8Colours else WithoutColours
    in
    if Help `elem` options then
      showUsage tc >> exitSuccess
    else if length dirNames /= 1 then do
      putTextLn "Please provide just one directory containing Elm source."
      showUsage tc
      exitFailure
    else do
      let dirName = Relude.Unsafe.head dirNames
      interoperate dirName >>= printResponse tc
      if Watch `elem` options then do
        putTextLn "Watching for changes..."
        withManager \mgr -> do
          _ <- watchTree mgr dirName  (isSuffixOf ".elm" . toText . eventPath) .
            const $ do
              putTextLn "\nRegenerating..."
              interoperate dirName >>= printResponse tc
              hFlush stdout
          forever $ threadDelay 1000000
      else pass

data Flag = Help | Watch | Colour deriving (Eq, Show)

cliOptions :: [OptDescr Flag]
cliOptions =
  [ GetOpt.Option [] ["help"] (NoArg Help) "Print the program information and usage."
  , GetOpt.Option ['w'] ["watch"] (NoArg Watch) "Watch for and regenerate on changes."
  , GetOpt.Option [] ["colour", "color"] (NoArg Colour) "Print coloured output."
  ]

showUsage :: TerminalCapabilities -> IO ()
showUsage tc = putChunksWith tc
    [ bold $ chunk "\nTIE - TypeScript Interoperator for Elm\n\n"
    , bold $ chunk "Usage: "
    , fore green . bold $ chunk "tie "
    , bold $ chunk "[OPTIONS...] DIRNAME\n\n"
    , bold $ chunk "Available options:\n"
    , bold $ chunk "        --help    "
    , chunk "Print this help message\n"
    , bold $ chunk "    -w  --watch   "
    , chunk "Watch for changes in the specified directory and regenerate\n"
    , chunk "                  definitions every time a change occurs\n"
    , bold $ chunk "        --colour  "
    , chunk "Print coloured output.\n"
    , bold $ chunk "        --color   "
    , chunk "Same as "
    , bold $ chunk "--colour.\n\n"
    ]

printResponse :: TerminalCapabilities -> Response Text (FilePath, Warnings) -> IO ()
printResponse tc = \case
  Ok (path, warnings) ->
    putChunksWith tc $ printWarnings warnings <>
      [ fore green . bold $ chunk "Done! You can see the generated type definitions at "
      , fore green . underline . chunk $ toText path <> "\n"
      ]
  Failed err -> do
    putChunksWith tc
      ((fore red . bold $ chunk "Some errors were encountered, so new TypeScript definitions were not generated:\n\n")
      : ((\l -> fore red . chunk $ "  - " <> l <> "\n") <$> lines err))

printWarnings :: Warnings -> [Chunk]
printWarnings = foldr printWarning []
  where printWarning w acc = (fore yellow . bold $ chunk "Warning: ") : fore yellow (chunk $ w <> "\n") : acc
