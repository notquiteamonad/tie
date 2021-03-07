{-# LANGUAGE BlockArguments #-}

module TIE.Lib
    ( interoperate
    ) where

import           Data.Text        (stripSuffix)
import           GHC.IO.Device    (IODeviceType (Directory))
import           System.Directory (createDirectoryIfMissing)
import           TIE.Elm.Main     (generateInitFunction)
import           TIE.Elm.Ports    (generatePortProperties)
import           TIE.Elm.Types    (findType)
import           TIE.FS           (getAllElmFilesIn, getMainElmFile)
import           TIE.TypeScript   (Document (Document),
                                   Exported (Exported, Private),
                                   Interface (Interface),
                                   InterfaceName (InterfaceName),
                                   Member (MPropertyGroup),
                                   Namespace (Namespace),
                                   NamespaceMember (NMInterface, NMNamespace),
                                   NamespaceName (NamespaceName),
                                   PropertyName (PropertyName), writeDocument)

interoperate :: FilePath -> IO ()
interoperate dirname = do
  elmFiles <- getAllElmFilesIn (dirname, Directory)
  (initFunction, neededCustomFlagType) <- generateInitFunction elmFiles
  (portProperties, neededCustomPortTypes) <- generatePortProperties elmFiles
  let neededCustomTypes = sortNub $ case neededCustomFlagType of
        Just ncft -> ncft : neededCustomPortTypes
        Nothing   -> neededCustomPortTypes
  additionalInterfaces <- forM neededCustomTypes (findType elmFiles)
  let mainFile = getMainElmFile elmFiles
  let dir = fromMaybe (error "Can't create output directory") $ stripSuffix ".elm" (toText mainFile)
  createDirectoryIfMissing True (toString dir)
  let outputFileName = dir <> "/index.d.ts"
  writeFile (toString outputFileName) . toString . writeDocument . Document $
    values initFunction additionalInterfaces portProperties
  putTextLn $ "Done! You can see the generated type definitions at " <> outputFileName
  where values initF additionalIs ports =
          [ Namespace Exported (NamespaceName "Elm")
              [ NMNamespace . Namespace Private (NamespaceName "Main") $
                [ NMInterface $ Interface Exported (InterfaceName "App")
                  [ MPropertyGroup (PropertyName "ports") ports
                  ]
                , initF
                ] <> (NMInterface <$> additionalIs)
              ]
          ]
