module TIE.Lib
    ( interoperate
    ) where

import           TIE.TypeScript

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
                , NMFunction $ Function (FunctionName "init")
                    []
                    (TInterface $ InterfaceName "Elm.Main.App")
                ]
              , NMInterface $ Interface Private (InterfaceName "UserInfo")
                  [ MProperty (PropertyName "emailAddress") $ TPrimitive PString
                  , MProperty (PropertyName "password") $ TPrimitive PString
                  ]
              ]
          ]
