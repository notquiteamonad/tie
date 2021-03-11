{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module TIE.Lib
    ( Response(..)
    , interoperate
    ) where

import           Data.Text        (stripSuffix)
import           GHC.IO.Device    (IODeviceType (Directory))
import           System.Directory (createDirectoryIfMissing)
import           TIE.Elm.Main     (generateInitFunction)
import           TIE.Elm.Ports    (generatePortProperties)
import           TIE.Elm.Types    (findType)
import           TIE.FS           (getAllElmFilesIn, getMainElmFile)
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

{-| Attempts to generate a TS definition file from the Elm files in the directory provided.
    Succeeds with `Ok PATH_TO_OUTPUT_DEFINITIONS` or fails with `Failed ERROR_MESSAGE`
-}
interoperate :: FilePath -> IO (Response Text FilePath)
interoperate dirname = do
  elmFiles <- getAllElmFilesIn (dirname, Directory)
  case getMainElmFile elmFiles of
    Ok mainFile -> generateInitFunction mainFile >>= \case
      Ok (initFunction, neededCustomFlagTypes) -> generatePortProperties elmFiles >>= \case
          Ok (portProperties, neededCustomPortTypes) -> do
            let neededCustomTypes = sortNub $ neededCustomFlagTypes <> neededCustomPortTypes
            additionalInterfaceResponses <- forM neededCustomTypes (findType elmFiles)
            let additionalInterfaces = catSuccesses additionalInterfaceResponses
            if length additionalInterfaces == length additionalInterfaceResponses then do
              case stripSuffix ".elm" $ toText mainFile of
                Just dir -> do
                  createDirectoryIfMissing True (toString dir)
                  let outputFileName = dir <> "/index.d.ts"
                  writeFile (toString outputFileName) . toString . writeDocument $
                    buildDocument initFunction additionalInterfaces portProperties
                  pure $ pure (toString outputFileName)
                Nothing -> pure $ Failed "Can't create output directory"
            else do
              pure . Failed . mconcat . intersperse "\n" $ catFailures additionalInterfaceResponses
          Failed e -> pure $ Failed e
      Failed e -> pure $ Failed e
    err -> pure err

buildDocument :: NamespaceMember -> [Interface] -> Members -> Document
buildDocument initFunction additionalInterfaces ports = Document
  [ Namespace (NamespaceName "Elm")
      [ NMNamespace Private . Namespace (NamespaceName "Main") $
        [ NMInterface $ Interface Exported (InterfaceName "App")
          [ MPropertyGroup (PropertyName "ports") ports ]
        , initFunction
        ] <> (NMInterface <$> additionalInterfaces)
      ]
  ]
