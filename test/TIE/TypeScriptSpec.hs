module TIE.TypeScriptSpec (spec) where

import           TIE.TypeScript
import           Test.Hspec

spec :: Spec
spec =
  describe "The TypeScript Module" do
      it "correctly formats a document" do
        putTextLn $ writeDocument testDocument
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
                        <> TPrimitive PNull
                        <> TPrimitive PString
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
