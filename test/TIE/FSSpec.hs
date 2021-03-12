module TIE.FSSpec (spec) where

import           System.FilePath
import           TIE.FS
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
  describe "getAllElmFilesIn" do
    it "gets all elm files below the directory specified" do
      elmFiles <- getAllElmFilesIn testDataDir
      sort elmFiles `shouldBe` sort
        [ testDataDir </> "Main.elm"
        , testDataDir </> "TestA.elm"
        , testDataDir </> "TestB.elm"
        , testDataDir </> "more-main-examples" </> "Main_Alias_Flags.elm"
        , testDataDir </> "more-main-examples" </> "Main_Invalid_Flags.elm"
        , testDataDir </> "more-main-examples" </> "Main_No_Type_Definition.elm"
        , testDataDir </> "more-main-examples" </> "Main_Unusual_Formatting.elm"
        , testDataDir </> "more-port-examples" </> "BadPort1.elm"
        , testDataDir </> "more-port-examples" </> "BadPort2.elm"
        ]
    it "just gets the one file if the argument is a regular elm file" do
      let filePath = testDataDir </> "Main.elm"
      elmFiles <- getAllElmFilesIn filePath
      elmFiles `shouldBe` [filePath]
    it "gets an empty list if the argument is a regular non-elm file" do
      let filePath = testDataDir </> "ANonElmFile.ts"
      elmFiles <- getAllElmFilesIn filePath
      elmFiles `shouldBe` []
    it "gets an empty list if the path given doesn't exist" do
      let filePath = testDataDir </> "somebadpath"
      elmFiles <- getAllElmFilesIn filePath
      elmFiles `shouldBe` []
