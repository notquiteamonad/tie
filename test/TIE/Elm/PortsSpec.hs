module TIE.Elm.PortsSpec (spec) where

import           System.FilePath ((</>))
import           TIE.Elm.Ports   (generatePortProperties)
import           TIE.Elm.Types   (NeededCustomType (NeededCustomType))
import           TIE.Response    (Response (Failed, Ok))
import           TIE.TypeScript  (Argument (Argument),
                                  ArgumentName (ArgumentName),
                                  Function (Function),
                                  FunctionName (FunctionName),
                                  Member (MFunction, MPropertyGroup),
                                  PrimitiveName (PNull, PString, PVoid),
                                  PropertyName (PropertyName),
                                  ReferenceName (ReferenceName),
                                  TSType (TArray, TFunction, TPrimitive, TReference))
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           TestUtils       (testDataDir)

-- Although three files are listed, the ports are all defined in TestA.elm.
testFilePaths :: [FilePath]
testFilePaths = (testDataDir </>) <$> [ "Main.elm", "TestA.elm", "TestB.elm" ]

morePortExamplesDir :: FilePath
morePortExamplesDir = testDataDir </> "more-port-examples"

spec :: Spec
spec = do
  describe "generatePortProperties" do
      it "can read a collection of valid ports" do
        ports <- generatePortProperties testFilePaths
        ports `shouldBe`
          Ok
            ( [ MPropertyGroup (PropertyName "port6")
                  [ MFunction .
                      Function (FunctionName "subscribe")
                        [ Argument (ArgumentName "callback") $
                            TFunction
                              [Argument (ArgumentName "data") $
                                TReference (ReferenceName "Elm.Main.Foo")
                              ]
                              (TPrimitive PVoid)
                        ] $ TPrimitive PVoid
                  ]
              , MPropertyGroup (PropertyName "port5")
                  [ MFunction .
                      Function (FunctionName "subscribe")
                        [ Argument (ArgumentName "callback") $
                            TFunction
                              [Argument (ArgumentName "data") $
                                TArray (TPrimitive PString)
                                <> TPrimitive PNull
                              ]
                              (TPrimitive PVoid)
                        ] $ TPrimitive PVoid
                  ]
              , MPropertyGroup (PropertyName "port4")
                  [ MFunction .
                      Function (FunctionName "subscribe")
                        [ Argument (ArgumentName "callback") $
                            TFunction
                              [Argument
                                (ArgumentName "data")
                                (TPrimitive PString)
                              ]
                              (TPrimitive PVoid)
                        ] $ TPrimitive PVoid
                  ]
              , MPropertyGroup (PropertyName "port3")
                  [ MFunction .
                      Function (FunctionName "send")
                        [ Argument (ArgumentName "data") $
                            TReference (ReferenceName "Elm.Main.Foo")
                        ] $ TPrimitive PVoid
                  ]
              , MPropertyGroup (PropertyName "port2")
                  [ MFunction .
                      Function (FunctionName "send")
                        [ Argument (ArgumentName "data") $
                            TArray (TPrimitive PString)
                            <> TPrimitive PNull
                        ] $ TPrimitive PVoid
                  ]
              , MPropertyGroup (PropertyName "port1")
                    [ MFunction $
                        Function (FunctionName "send")
                          [ Argument (ArgumentName "data") (TPrimitive PString) ]
                          (TPrimitive PVoid)
                    ]
            ]
            , [NeededCustomType "Elm.Main.Foo", NeededCustomType "Elm.Main.Foo"]
            )
      it "fails if the port contains a bad type" do
        ports <- generatePortProperties [morePortExamplesDir </> "BadPort1.elm"]
        ports `shouldBe`
          Failed (
            "Could not parse type (( -> msg) -> Sub msg in port definition "
            <> "port badPort1 : (( -> msg) -> Sub msg"
          )
      it "fails if the port definition is incomplete" do
        ports <- generatePortProperties [morePortExamplesDir </> "BadPort2.elm"]
        ports `shouldBe`
          Failed (
            "Could not parse incomplete port definition: "
            <> "port"
          )
