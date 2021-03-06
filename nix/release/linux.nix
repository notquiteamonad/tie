let
  tie = import ../../default.nix { };
  pkgs = import <nixpkgs> { };
in
tie.projectCross.musl64.hsPkgs.tie.components.exes.tie-linux.override {
  configureFlags = [
    "--disable-executable-dynamic"
    "--disable-shared"
    "--ghc-option=-optl=-pthread"
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
  ];
}
