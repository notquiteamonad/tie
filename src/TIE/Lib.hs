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
                                 PrimitiveName (PNull, PString, PVoid),
                                 PropertyName (PropertyName),
                                 TSType (TInlineInterface, TInterface, TPrimitive),
                                 writeDocument)

interoperate :: IO ()
interoperate = putTextLn . writeDocument $ Document values
  where values =
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
                , NMFunction Exported $ Function (FunctionName "init")
                    [ Argument (ArgumentName "options") $ TInlineInterface
                      [ MProperty (PropertyName "node?") (TInterface (InterfaceName "HTMLElement") <> TPrimitive PNull)
                      ]
                    ]
                    (TInterface $ InterfaceName "Elm.Main.App")
                ]
              ]
          ]
