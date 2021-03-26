{ haskellNix ? import
    (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/72beef11fc6ec32a98f1dd0d4dcd072c89595b43.tar.gz")
    { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "tie";
    src = ./.;
  };
  stack-sha256 = "1r8g2jfa129cravfig96armk439fcm1y5152xhfqbyav152rzi7g";
  materialized = ./tie.materialized;
  checkMaterialization = true;
}
