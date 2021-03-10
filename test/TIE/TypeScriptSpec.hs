module TIE.TypeScriptSpec (spec) where

import           TIE.TypeScript (Argument (Argument),
                                 ArgumentName (ArgumentName), Document (..),
                                 Exported (Exported, Private),
                                 Function (Function),
                                 FunctionName (FunctionName),
                                 Interface (Interface),
                                 InterfaceName (InterfaceName),
                                 Member (MFunction, MProperty, MPropertyGroup),
                                 Namespace (Namespace),
                                 NamespaceMember (NMFunction, NMInterface, NMNamespace),
                                 NamespaceName (NamespaceName),
                                 PrimitiveName (PBoolean, PNull, PNumber, PString, PUnknown, PVoid),
                                 PropertyName (PropertyName),
                                 TSType (TArray, TFunction, TInlineInterface, TInterface, TPrimitive),
                                 writeDocument)
import           Test.Hspec     (Spec, it, shouldBe)

spec :: Spec
spec =
  it "correctly formats a document" do
    lines (writeDocument testDocument) `shouldBe` testDocumentOutput

testDocument :: Document
testDocument = Document
  [ Namespace (NamespaceName "TopLevelNS1")
      [ NMNamespace Private $ Namespace (NamespaceName "InnerNS1") []
      , NMNamespace Exported $ Namespace (NamespaceName "InnerNS2") []
      , NMInterface $ Interface Exported (InterfaceName "NSInterface1")
          [ MPropertyGroup (PropertyName "myGroup")
              [ MPropertyGroup (PropertyName "innerEmptyGroup") []
              , MProperty (PropertyName "pInterface") $ TInterface (InterfaceName "Some.Package.Interface")
              , MFunction $
                  Function
                    (FunctionName "foo")
                    [ Argument (ArgumentName "bar") . TArray $ TArray (TPrimitive PString <> TPrimitive PNumber)
                    , Argument (ArgumentName "baz") $ TPrimitive PUnknown
                    , Argument (ArgumentName "additionalData") $
                        TInlineInterface [MProperty (PropertyName "isGoodData") $ TPrimitive PBoolean]
                        <> TPrimitive PString
                        <> TPrimitive PNull
                    ]
                    (TPrimitive PVoid)
              , MFunction $
                  Function
                    (FunctionName "hof")
                    [ Argument (ArgumentName "f") $ TFunction
                        [ Argument ( ArgumentName "s" ) $ TPrimitive PNull <> TPrimitive PVoid <> TPrimitive PNull ]
                        ((TPrimitive PNull <> TPrimitive PNull) <> (TPrimitive PNull <> TPrimitive PNull))
                    ]
                    (TPrimitive PVoid)
              ]
          ]
      , NMInterface $ Interface Private (InterfaceName "NSInterface2") []
      , NMFunction Exported . Function (FunctionName "topLevelFunction1") [] $ TPrimitive PVoid
      , NMFunction Private . Function (FunctionName "topLevelFunction2")  [] $ TPrimitive PString
      ]
  , Namespace (NamespaceName "TopLevelNS2") []
  ]

testDocumentOutput :: [Text]
testDocumentOutput =
  [ "export namespace TopLevelNS1 {"
  , "  namespace InnerNS1 {"
  , "  }"
  , "  export namespace InnerNS2 {"
  , "  }"
  , "  export interface NSInterface1 {"
  , "    myGroup: {"
  , "      innerEmptyGroup: {"
  , "      };"
  , "      pInterface: Some.Package.Interface;"
  , "      foo(bar: (string[] | number[])[], baz: unknown, additionalData: { isGoodData: boolean } | string | null): void;"
  , "      hof(f: (s: void | null) => null): void;"
  , "    };"
  , "  }"
  , "  interface NSInterface2 {"
  , "  }"
  , "  export function topLevelFunction1(): void;"
  , "  function topLevelFunction2(): string;"
  , "}"
  , "export namespace TopLevelNS2 {"
  , "}"
  , ""
  ]