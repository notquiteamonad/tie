module TIE.Lib
    ( interoperate
    ) where

import           GHC.IO.Device  (IODeviceType (Directory))
import           TIE.Elm.Main   (generateInitFunction)
import           TIE.Elm.Ports  (generatePortProperties)
import           TIE.Elm.Types  (findType)
import           TIE.FS         (getAllElmFilesIn)
import           TIE.TypeScript (Document (Document),
                                 Exported (Exported, Private),
                                 Interface (Interface),
                                 InterfaceName (InterfaceName),
                                 Member (MPropertyGroup), Namespace (Namespace),
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
  putTextLn . writeDocument . Document $ values initFunction additionalInterfaces portProperties
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
