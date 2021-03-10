module TIE.Elm.Internal.ExpressionSpec (spec) where

import           TIE.Elm.Internal.Expression (bracketedExpression)
import           Test.Hspec                  (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "bracketedExpression" do
    it "returns nothing if the brackets are not properly matched" do
      bracketedExpression "()" `shouldBe` Nothing
      bracketedExpression "String" `shouldBe` Nothing
      bracketedExpression ")))" `shouldBe` Nothing
      bracketedExpression "(" `shouldBe` Nothing
    it "can parse a single-word type" do
      bracketedExpression "String)" `shouldBe` Just "String"
      bracketedExpression "Int)" `shouldBe` Just "Int"
      bracketedExpression "(Char))" `shouldBe` Just "Char"
    it "can strip any number of properly matched brackets" do
      bracketedExpression "(((((String))))))" `shouldBe` Just "String"
