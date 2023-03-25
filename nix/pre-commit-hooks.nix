{
  nixpkgs,
  pre-commit-hooks-lib,
  system,
}: let
  buildPreCommitHook = hooks:
    pre-commit-hooks-lib.lib.${system}.run {
      src = ../.;
      inherit hooks;
    };
  pureHooks = {
    alejandra.enable = true;
    hlint.enable = true;
    hpack = {
      enable = true;
      files = nixpkgs.lib.mkOverride 0 "(\\.l?hs$)|cabal\\.project|([^/]+\\.cabal$)|(package\\.yaml$)";
    };
    ormolu.enable = true;
  };
  impureHooks = {
    z_build_tie = {
      enable = true;
      entry = "${nixpkgs.writeShellScriptBin "build-tie" "nix build '.#tie'"}/bin/build-tie";
      files = "(\\.l?hs$)|cabal\\.project|([^/]+\\.cabal$)|(package\\.yaml$)";
      name = "build tie";
      pass_filenames = false;
    };
  };
in {
  pureHooks = buildPreCommitHook pureHooks;
  allHooks = buildPreCommitHook (pureHooks // impureHooks);
  tools = with pre-commit-hooks-lib.packages.${system}; [
    alejandra
    hlint
    hpack
    ormolu
  ];
}
