module TIE.Lib
    ( interoperate
    ) where

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
                                 NamespaceMember (NMFunction, NMInterface, NMNamespace),
                                 NamespaceName (NamespaceName),
                                 PrimitiveName (PString, PVoid),
                                 PropertyName (PropertyName),
                                 TSType (TInterface, TPrimitive), writeDocument)

interoperate :: IO ()
interoperate = putTextLn . writeDocument $ Document values
  where values =
          [ Namespace Exported (NamespaceName "Elm")
              [ NMNamespace $ Namespace Private (NamespaceName "Main")
                [ NMInterface $ Interface Exported (InterfaceName "App")
                  [ MPropertyGroup (PropertyName "ports")
                    [ MPropertyGroup (PropertyName "handleSignIn")
                      [ MFunction $ Function (FunctionName "send")
                        [ Argument (ArgumentName "data") . TInterface $ InterfaceName "Elm.Main.UserInfo"
                        ]
                        (TPrimitive PVoid)
                      ]
                    ]
                  ]
                , NMFunction Exported $ Function (FunctionName "init")
                    []
                    (TInterface $ InterfaceName "Elm.Main.App")
                , NMInterface $ Interface Exported (InterfaceName "UserInfo")
                    [ MProperty (PropertyName "emailAddress") $ TPrimitive PString
                    , MProperty (PropertyName "password") $ TPrimitive PString
                    ]
                ]
              ]
          ]
