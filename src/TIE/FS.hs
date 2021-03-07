module TIE.FS
  ( getAllElmFilesIn
  , getMainElmFile
  ) where

import           Data.Text              (isSuffixOf)
import           GHC.IO.Device          (IODeviceType (Directory, RegularFile))
import           System.Directory       (listDirectory)
import           System.Posix.Internals (fileType)

{-| Gets a list of FilePaths that end in ".elm" under the given directory.
-}
getAllElmFilesIn :: (FilePath, IODeviceType) -> IO [FilePath]
getAllElmFilesIn (f, t) = case t of
  Directory -> do
    filesInDir <- listDirectory f
    if null filesInDir then pure []
    else do
      typedFilesInDir <- forM ((dirName <>) <$> filesInDir) fileTypeTuple
      mconcat $ getAllElmFilesIn <$> typedFilesInDir
    where dirName = if "/" `isSuffixOf` toText f then f else f <> "/"
  RegularFile ->
    pure [elmFile | ".elm" `isSuffixOf` toText f, elmFile <- [f]]
  _ -> pure []

getMainElmFile :: [FilePath] -> FilePath
getMainElmFile paths = fromMaybe (error "Could not find a Main.elm in the directory given") .
                        viaNonEmpty head $ filter (\path -> "Main.elm" `isSuffixOf` toText path) paths


fileTypeTuple :: FilePath -> IO (FilePath, IODeviceType)
fileTypeTuple f = do
  t <- fileType f
  pure (f, t)
