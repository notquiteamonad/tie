module TIE.ResponseSpec (spec) where

import           TIE.Response          (Response (..), catFailures,
                                        catResponses, catSuccesses)
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "catResponses" do
    it "returns the first failed value if there is one" do
      catResponses
        ([ Ok 1, Ok 2, Failed 3, Ok 4 ] :: [Response Int Int])
        `shouldBe`
        Failed 3
    it "returns an ok response, having concatenated the ok values, if there were no failed values" do
      catResponses
        ([ Ok 1, Ok 2, Ok 3 ] :: [Response Int Int])
        `shouldBe`
        Ok [1, 2, 3]
  describe "catSuccesses" do
    it "is empty if the input is empty" do
      catSuccesses ([] :: [Response () ()]) `shouldBe` []
    it "only returns the Ok values" do
      catSuccesses
        ([ Ok 1, Ok 2, Failed 3, Ok 4 ] :: [Response Int Int])
        `shouldBe`
        [1, 2, 4]
    prop "is idempotent" do
      \x -> let successes = Ok <$> (x :: [Int])
            in catSuccesses successes `shouldBe` x
  describe "catFailures" do
    it "is empty if the input is empty" do
      catFailures ([] :: [Response () ()]) `shouldBe` []
    it "only returns the Failed values" do
      catFailures
        ([ Ok 1, Ok 2, Failed 3, Ok 4 ] :: [Response Int Int])
        `shouldBe`
        [3]
    prop "is idempotent" do
      \x -> let failures = Failed <$> (x :: [Int])
            in catFailures failures `shouldBe` x
