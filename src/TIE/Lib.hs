module TIE.Lib
    ( interoperate
    ) where

import           GHC.IO.Device  (IODeviceType (Directory))
import           TIE.Elm.Main   (generateInitFunction)
import           TIE.FS         (getAllElmFilesIn)
import           TIE.TypeScript (Argument (Argument),
                                 ArgumentName (ArgumentName),
                                 Document (Document),
                                 Exported (Exported, Private),
                                 Function (Function),
                                 FunctionName (FunctionName),
                                 Interface (Interface),
                                 InterfaceName (InterfaceName),
                                 Member (MFunction, MProperty, MPropertyGroup),
                                 Namespace (Namespace),
                                 NamespaceMember (NMInterface, NMNamespace),
                                 NamespaceName (NamespaceName),
                                 PrimitiveName (PString, PVoid),
                                 PropertyName (PropertyName),
                                 TSType (TInlineInterface, TPrimitive),
                                 writeDocument)

interoperate :: FilePath -> IO ()
interoperate dirname = do
  elmFiles <- getAllElmFilesIn (dirname, Directory)
  initFunction <- generateInitFunction elmFiles
  putTextLn . writeDocument . Document $ values initFunction
    where values initF =
            [ Namespace Exported (NamespaceName "Elm")
                [ NMNamespace $ Namespace Private (NamespaceName "Main")
                  [ NMInterface $ Interface Exported (InterfaceName "App")
                    [ MPropertyGroup (PropertyName "ports")
                      [ MPropertyGroup (PropertyName "handleSignIn")
                        [ MFunction $ Function (FunctionName "send")
                          [ Argument (ArgumentName "data") $ TInlineInterface
                            [ MProperty (PropertyName "emailAddress") $ TPrimitive PString
                            , MProperty (PropertyName "password") $ TPrimitive PString
                            ]
                          ]
                          (TPrimitive PVoid)
                        ]
                      , MPropertyGroup (PropertyName "handleSignOut")
                        [ MFunction $ Function (FunctionName "subscribe") [] (TPrimitive PVoid)
                        ]
                      ]
                    ]
                  , initF
                  ]
                ]
            ]
