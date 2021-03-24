{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "tie"; version = "0.1.1.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright notquiteamonad 2021";
      maintainer = "44178347+notquiteamonad@users.noreply.github.com";
      author = "notquiteamonad";
      homepage = "https://github.com/notquiteamonad/tie#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/notquiteamonad/tie#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" "LICENSE" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
          ];
        buildable = true;
        modules = [ "Paths_tie" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "tie-exe" = {
          depends = [
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."safe-coloured-text" or (errorHandler.buildDepError "safe-coloured-text"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tie" or (errorHandler.buildDepError "tie"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            ];
          buildable = true;
          modules = [ "Paths_tie" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "tie-test" = {
          depends = [
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tie" or (errorHandler.buildDepError "tie"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            ];
          buildable = true;
          modules = [ "Paths_tie" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }) // {
    cabal-generator = "hpack";
    }