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
  stack-sha256 = "1vp8xwm5rrgvy62qccqcqf2zfv9k74p4rhlykb4m35669sqz60z9";
  materialized = ./nix/tie.materialized;
  checkMaterialization = true;
}
