module TIE.FS
  ( getAllElmFilesIn
  , getMainElmFile
  ) where

import           Data.Text              (isSuffixOf)
import           GHC.IO.Device          (IODeviceType (Directory, RegularFile))
import           System.Directory       (listDirectory)
import           System.FilePath        ((</>))
import           System.IO.Error        (catchIOError)
import           System.Posix.Internals (fileType)
import           TIE.Response           (Response (..))

{-| Gets a list of FilePaths that end in ".elm" under the given directory.
-}
getAllElmFilesIn :: FilePath -> IO [FilePath]
getAllElmFilesIn topDir = do
  mFileTypeTuple <- catchIOError (Just <$> fileTypeTuple topDir) (const $ pure Nothing)
  maybe (pure []) go mFileTypeTuple
  where
    go :: (FilePath, IODeviceType) -> IO [FilePath]
    go (f, t) = case t of
      Directory -> do
        filesInDir <- listDirectory f
        if null filesInDir then pure []
        else do
          typedFilesInDir <- forM ((f </>) <$> filesInDir) fileTypeTuple
          mconcat $ go <$> typedFilesInDir
      RegularFile ->
        pure [elmFile | ".elm" `isSuffixOf` toText f, elmFile <- [f]]
      _ -> pure []

fileTypeTuple :: FilePath -> IO (FilePath, IODeviceType)
fileTypeTuple f = do
  t <- fileType f
  pure (f, t)

getMainElmFile :: [FilePath] -> Response Text FilePath
getMainElmFile paths =
  case viaNonEmpty head $ filter (\path -> "Main.elm" `isSuffixOf` toText path) paths of
    Just mainPath -> pure mainPath
    Nothing       -> Failed "Could not find a Main.elm in the directory given"

