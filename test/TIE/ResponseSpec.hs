module TIE.ResponseSpec (spec) where

import           TIE.Response          (Response (..), catFailures,
                                        catSuccessess)
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "catSuccesses" do
    it "is empty if the input is empty" do
      catSuccessess ([] :: [Response () ()]) `shouldBe` []
    it "only returns the Ok values" do
      catSuccessess
        ([ Ok 1, Ok 2, Failed 3, Ok 4 ] :: [Response Int Int])
        `shouldBe`
        [1, 2, 4]
    prop "is idempotent" do
      \x -> let successes = Ok <$> (x :: [Int])
            in catSuccessess successes `shouldBe` x
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
