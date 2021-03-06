module TIE.Elm.InitSpec (spec) where

import           System.FilePath ((</>))
import           TIE.Elm.Init    (generateInitFunction)
import           TIE.Elm.Types   (NeededCustomType (NeededCustomType))
import           TIE.Response    (Response (Failed, Ok))
import           TIE.TypeScript  (Argument (Argument),
                                  ArgumentName (ArgumentName),
                                  Exported (Exported), Function (Function),
                                  FunctionName (FunctionName),
                                  Member (MProperty),
                                  NamespaceMember (NMFunction),
                                  PrimitiveName (PNull, PNumber, PString),
                                  PropertyName (PropertyName),
                                  ReferenceName (ReferenceName),
                                  TSType (TInlineInterface, TPrimitive, TReference))
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           TestUtils       (testDataDir)

moreMainExamplesDir :: FilePath
moreMainExamplesDir = testDataDir </> "more-main-examples"

mainFunctionWithFlags :: [Member] -> [NeededCustomType] -> Response Text (NamespaceMember, [NeededCustomType])
mainFunctionWithFlags flagsMembers neededCustomTypes =
  Ok (
    NMFunction Exported
      (Function
        (FunctionName "init")
        [ Argument (ArgumentName "options") . TInlineInterface $
            MProperty (PropertyName "node?") (TReference (ReferenceName "HTMLElement") <> TPrimitive PNull)
            : flagsMembers
        ]
        (TReference $ ReferenceName "Elm.Main.App")
      )
    , neededCustomTypes
    )

spec :: Spec
spec = do
  describe "generateInitFunction" do
    it "Reads flags from a normal main function containing utf16 chars" do
      flags <- generateInitFunction (testDataDir </> "Main.elm")
      flags `shouldBe` mainFunctionWithFlags [MProperty (PropertyName "flags") $ TPrimitive PString] []
    it "Reads flags from an unusually formatted main function" do
      flags <- generateInitFunction (moreMainExamplesDir </> "Main_Unusual_Formatting.elm")
      flags `shouldBe` mainFunctionWithFlags
        [MProperty (PropertyName "flags") $ TPrimitive PNumber <> TPrimitive PNull] []
    it "Reads flags of a non-literal type" do
      flags <- generateInitFunction (moreMainExamplesDir </> "Main_Alias_Flags.elm")
      flags `shouldBe` mainFunctionWithFlags [MProperty (PropertyName "flags") $ TReference (ReferenceName "Elm.Main.S")] [NeededCustomType "Elm.Main.S"]
    it "Returns a Failed response if the flags type definition is invalid" do
      flags <- generateInitFunction (moreMainExamplesDir </> "Main_Invalid_Flags.elm")
      flags `shouldBe` Failed "Could not read flags type from main definition"
    it "Returns a Failed response if the main definition has no explicit type definition" do
      flags <- generateInitFunction (moreMainExamplesDir </> "Main_No_Type_Definition.elm")
      flags `shouldBe` Failed ("Could not find main's type definition. "
        <> "Are you specifying it explicitly?")

