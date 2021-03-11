module TIE.Elm.ExpressionSpec (spec) where

import           TIE.Elm.Expression (readNextExpression)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "readNextExpression" do
    it "can read a unit expression" do
      readNextExpression "()" `shouldBe` Just "()"
    it "can read a single-word type" do
      readNextExpression "String" `shouldBe` Just "String"
      readNextExpression "Int" `shouldBe` Just "Int"
      readNextExpression "MyType" `shouldBe` Just "MyType"
    it "can read a multi-word type, when bracketed" do
      readNextExpression "(Maybe String)" `shouldBe` Just "Maybe String"
      readNextExpression "(Result a b)" `shouldBe` Just "Result a b"
      readNextExpression "((Maybe String))" `shouldBe` Just "Maybe String"
      readNextExpression "(Maybe ())" `shouldBe` Just "Maybe ()"
    it "returns Nothing for an invalid type" do
      readNextExpression "(()" `shouldBe` Nothing
