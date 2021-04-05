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
  stack-sha256 = "0ax8bx84wj5w8zn4992dsx383nz2fh5wh8ygqsrd94b1bnk5mmrz";
  materialized = ./nix/tie.materialized;
  checkMaterialization = true;
}
