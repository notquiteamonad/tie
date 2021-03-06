{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

{-|
Module: TIE.Lib

The entrypoint for TIE executables.
-}
module TIE.Lib
    ( Response(..)
    , Warnings
    , interoperate
    ) where

import           Data.Text        (stripSuffix)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))
import           TIE.Config       (cfgOverrides, getConfig)
import           TIE.Elm.Init     (generateInitFunction)
import           TIE.Elm.Ports    (generatePortProperties)
import           TIE.Elm.Types    (NeededCustomType, findType)
import           TIE.FS           (getAllElmFilesIn, getMainElmFile)
import           TIE.Override     (overrideMembers)
import           TIE.Response     (Response (..), catFailures, catSuccesses)
import           TIE.TypeScript   (Document (Document),
                                   Exported (Exported, Private),
                                   Interface (Interface),
                                   InterfaceName (InterfaceName),
                                   Member (MPropertyGroup), Members,
                                   Namespace (Namespace),
                                   NamespaceMember (NMInterface, NMNamespace),
                                   NamespaceName (NamespaceName),
                                   PropertyName (PropertyName), writeDocument)

-- |A list of warnings to be displayed to the user.
type Warnings = [Text]

{-|
  Attempts to generate a TS definition file from the Elm files in the directory provided.

  Succeeds with "`Ok` (PATH_TO_OUTPUT_DEFINITIONS, WARNINGS)" or fails with "`Failed` ERROR_MESSAGE"
-}
interoperate :: FilePath -> IO (Response Text (FilePath, Warnings))
interoperate dirname = getConfig >>= \case
  Ok (config, defaultConfigWarning) -> do
    elmFiles <- getAllElmFilesIn dirname
    case getMainElmFile elmFiles of
      Ok mainFile -> generateInitFunction mainFile >>= \case
        Ok (initFunction, neededCustomFlagTypes) -> generatePortProperties elmFiles >>= \case
            Ok (portProperties, neededCustomPortTypes) -> do
              let neededCustomTypes = neededCustomFlagTypes <> neededCustomPortTypes
              getAdditionalNamespaceMembers elmFiles neededCustomTypes >>= \case
                Ok additionalNamespaceMembers ->
                  let (overriddenAdditionalNamespaceMembers, overrideWarnings, overrideMessages)
                        = overrideMembers additionalNamespaceMembers (cfgOverrides config)
                  in
                  forM_ overrideMessages putTextLn >>
                  case stripSuffix ".elm" $ toText mainFile of
                    Just dir -> do
                      createDirectoryIfMissing True (toString dir)
                      let outputFileName = toString dir </> "index.d.ts"
                      writeFile outputFileName . toString . writeDocument $
                        buildDocument (initFunction : overriddenAdditionalNamespaceMembers) portProperties
                      pure $ pure (toString outputFileName, catMaybes [defaultConfigWarning] <> overrideWarnings)
                    Nothing -> pure $ Failed "Can't create output directory"
                Failed e -> pure $ Failed e
            Failed e -> pure $ Failed e
        Failed e -> pure $ Failed e
      Failed e -> pure $ Failed e
  Failed e -> pure $ Failed e

-- |Recursively calls `findType` until all needed custom types are found.
getAdditionalNamespaceMembers :: [FilePath] -> [NeededCustomType] -> IO (Response Text [NamespaceMember])
getAdditionalNamespaceMembers elmFiles xs = go xs xs []
  where
    go :: [NeededCustomType] -> [NeededCustomType] -> [NamespaceMember] -> IO (Response Text [NamespaceMember])
    go ncts seenNcts acc = do
      additionalNamespaceMemberResponses <- forM (sortNub ncts) (findType elmFiles)
      let additionalNamespaceMembers = catSuccesses additionalNamespaceMemberResponses
      if length additionalNamespaceMembers == length additionalNamespaceMemberResponses then do
        let (newMembers, newNcts) = concat <$> unzip additionalNamespaceMembers
        if all (`elem` seenNcts) newNcts then
          pure . pure . sortNub $ acc <> newMembers
        else
          go newNcts (newNcts <> seenNcts) (acc <> newMembers)
      else do
        pure . Failed . mconcat . intersperse "\n" $ catFailures additionalNamespaceMemberResponses

buildDocument :: [NamespaceMember] -> Members -> Document
buildDocument additionalNamespaceMembers ports = Document
  [ Namespace (NamespaceName "Elm")
      [ NMNamespace Private . Namespace (NamespaceName "Main") $
        [ NMInterface $ Interface Exported (InterfaceName "App")
          [ MPropertyGroup (PropertyName "ports") ports ]
        ] <> additionalNamespaceMembers
      ]
  ]
