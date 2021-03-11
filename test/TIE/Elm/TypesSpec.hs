{-# LANGUAGE TemplateHaskell #-}

module TIE.Elm.TypesSpec (spec) where

import qualified Data.Text             as T
import           Language.Haskell.TH   (listE, litE, runIO, stringL)
import           System.Directory      (getCurrentDirectory)
import           System.FilePath       ((</>))
import           TIE.Elm.Types         (ElmType (CustomType, ElmArrayType, ElmPrimitiveType),
                                        NeededCustomType (NeededCustomType),
                                        elmTypeFromText, findType)
import           TIE.Response          (Response (Failed, Ok))
import           TIE.TypeScript        (Exported (Exported),
                                        Interface (Interface),
                                        InterfaceName (InterfaceName),
                                        Member (MProperty),
                                        PrimitiveName (PBoolean, PNull, PNumber, PString, PUnknown, PVoid),
                                        PropertyName (PropertyName),
                                        TSType (TInterface, TPrimitive))
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

testFilePaths :: [FilePath]
-- testFilePaths = undefined
testFilePaths = $(runIO getCurrentDirectory >>= \dir -> listE $
                    (\p -> litE . stringL $ dir </> "test" </> "test-data" </> p)
                    <$> [ "TestA.elm", "TestB.elm" ])

spec :: Spec
spec = do
  describe "elmTypeFromText" do
    it "can parse the String primitive" do
      elmTypeFromText "String" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PString))
    it "can parse the Int primitive" do
      elmTypeFromText "Int" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PNumber))
    it "can parse the Float primitive" do
      elmTypeFromText "Float" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PNumber))
    it "can parse the Bool primitive" do
      elmTypeFromText "Bool" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PBoolean))
    it "can parse JSON values" do
      elmTypeFromText "Value" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PUnknown))
      elmTypeFromText "Json.Decode.Value" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PUnknown))
      elmTypeFromText "Json.Encode.Value" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PUnknown))
    it "can parse the unit literal" do
      elmTypeFromText "()" `shouldBe` Ok (ElmPrimitiveType (TPrimitive PNull))
    it "can parse maybe values of literals" do
      elmTypeFromText "Maybe String" `shouldBe`
        Ok (ElmPrimitiveType (TPrimitive PString <> TPrimitive PVoid <> TPrimitive PNull ))
      elmTypeFromText "Maybe ()" `shouldBe`
        Ok (ElmPrimitiveType (TPrimitive PVoid <> TPrimitive PNull))
    it "can parse maybe values of custom interfaces" do
      elmTypeFromText "Maybe Foo" `shouldBe`
        Ok ( CustomType
              (TInterface (InterfaceName "Elm.Main.Foo") <> TPrimitive PVoid <> TPrimitive PNull)
              (NeededCustomType "Elm.Main.Foo")
           )
    it "can parse a list of literals" do
      elmTypeFromText "List String" `shouldBe` Ok (ElmArrayType (ElmPrimitiveType $ TPrimitive PString))
      elmTypeFromText "List ()" `shouldBe` Ok (ElmArrayType (ElmPrimitiveType $ TPrimitive PNull))
    it "can parse a list of maybe values" do
      elmTypeFromText "List (Maybe String)" `shouldBe`
        Ok (ElmArrayType (ElmPrimitiveType $ TPrimitive PString <> TPrimitive PVoid <> TPrimitive PNull))
    it "can parse a list of custom types" do
      elmTypeFromText "List Foo" `shouldBe`
        Ok (ElmArrayType
              (CustomType
                (TInterface (InterfaceName "Elm.Main.Foo"))
                (NeededCustomType "Elm.Main.Foo")
              )
           )
    it "can parse a complex nesting of lists and maybes" do
      elmTypeFromText "List (List (Maybe (List Foo)))" `shouldBe`
        Ok (ElmArrayType
              (ElmArrayType $
                ElmArrayType
                  (CustomType
                    (TInterface (InterfaceName "Elm.Main.Foo"))
                    (NeededCustomType "Elm.Main.Foo")
                  )
                <> ElmPrimitiveType (TPrimitive PVoid <> TPrimitive PNull)
              )
           )
    it "can parse array values" do
      elmTypeFromText "Array String" `shouldBe` Ok (ElmArrayType (ElmPrimitiveType $ TPrimitive PString))
    prop "treats lists and arrays interchangeably" do
      \x ->
        let t = toText (x :: String)
            l = elmTypeFromText ("List " <> t)
            a = elmTypeFromText ("Array " <> t)
        in if not . null $ words t then
          case (l, a) of
            (Ok l', Ok a') -> a' `shouldBe` l'
            (Failed l', Failed a') ->
              T.stripPrefix "Could not parse type Array " a'
              `shouldBe`
              T.stripPrefix "Could not parse type List " l'
            (l', a') -> fail $
              "The list variant was " <> show l' <> " but the array variant was " <> show a'
        else pass
    it "parses as a custom type for everything else" do
      elmTypeFromText "Foo" `shouldBe`
        Ok ( CustomType
              (TInterface (InterfaceName "Elm.Main.Foo"))
              (NeededCustomType "Elm.Main.Foo")
           )
    it "returns a Failed response for an invalid expression" do
      elmTypeFromText "Maybe (()" `shouldBe` Failed "Could not parse type Maybe (()"
  describe "findType" do
    it "can find a simple record type" do
      foo <- findType testFilePaths (NeededCustomType "Foo")
      foo `shouldBe` Ok
        (Interface Exported (InterfaceName "Foo")
          [ MProperty (PropertyName "username") (TPrimitive PString)
          , MProperty (PropertyName "email") (TPrimitive PString)
          ]
        )
    it "can find a strangely-formatted complex record type" do
      foo <- findType testFilePaths (NeededCustomType "Bar")
      foo `shouldBe` Ok
        (Interface Exported (InterfaceName "Bar")
          [ MProperty (PropertyName "userId") (TPrimitive PNumber)
          , MProperty (PropertyName "username") (TPrimitive PString)
          , MProperty (PropertyName "email") (TPrimitive PString <> TPrimitive PVoid <> TPrimitive PNull)
          , MProperty (PropertyName "bar") (TInterface (InterfaceName "Elm.Main.Bar"))
          ]
        )
