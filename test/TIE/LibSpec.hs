module TIE.LibSpec (spec) where

import           Data.Text.IO     (hGetContents)
import           GHC.IO           (throwIO)
import           System.Directory (removeFile)
import           System.FilePath  ((</>))
import           System.IO.Error  (catchIOError, isDoesNotExistError)
import           TIE.Lib          (interoperate)
import           Test.Hspec       (Spec, it, shouldBe)
import           TestUtils        (testDataDir)

spec :: Spec
spec = do
  it "Generates TypeScript definitions for a small valid project" do
    let projectDir = testDataDir </> "small-valid-project"
    let definitionsDir = projectDir </> "Main"
    let [definitionsFile, expectedDefinitionsFile] = (definitionsDir </>) <$> ["index.d.ts", "index.expected.d.ts"]
    removeIfExists definitionsFile
    _ <- interoperate projectDir
    withFile definitionsFile ReadMode \h1 ->
      withFile expectedDefinitionsFile ReadMode \h2 -> do
        actual <- hGetContents h1
        expected <- hGetContents h2
        actual `shouldBe` expected

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = catchIOError (removeFile fileName) handleExists
  where handleExists e
          | isDoesNotExistError e = pass
          | otherwise = throwIO e
