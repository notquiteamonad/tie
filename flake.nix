{
  description = "Development infrastructure for tie";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs?rev=a1291d0d020a200c7ce3c48e96090bfa4890a475";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks-lib = {
      inputs.flake-utils.follows = "flake-utils";
      url = "github:cachix/pre-commit-hooks.nix";
    };
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    pre-commit-hooks-lib,
    self,
  }:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems
    (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      pre-commit-hooks = import nix/pre-commit-hooks.nix {
        inherit nixpkgs pre-commit-hooks-lib system;
      };
      haskellPackages = nixpkgs.haskell.packages.ghc924;
      haskellEnv = haskellPackages.ghcWithHoogle (
        haskellPackages:
          with haskellPackages; [
            cabal-install
            haskell-language-server
            implicit-hie
          ]
      );
      tieUnstripped = nixpkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "tie" ./. {}) (
        super: {
          configureFlags = (super.configureFlags or []) ++ ["-fprod"];
        }
      );
      tie =
        nixpkgs.stdenv.mkDerivation
        {
          inherit (tieUnstripped) pname src version;
          buildInputs = with nixpkgs; [
            glibc
            gmp
            libffi
            ncurses
            zlib
          ];
          installPhase = ''
            mkdir -p $out/bin
            cp ${(nixpkgs.haskell.lib.enableSeparateBinOutput tieUnstripped).bin}/bin/tie-exe $out/bin/tie
          '';
        };
    in {
      checks = {pre-commit-check = pre-commit-hooks.pureHooks;};
      devShells.default = nixpkgs.mkShell {
        inherit (pre-commit-hooks.allHooks) shellHook;
        buildInputs = pre-commit-hooks.tools ++ [haskellEnv nixpkgs.rnix-lsp];
      };
      packages = {
        inherit tieUnstripped tie;
        default = self.packages.${system}.tie;
        # runChecks is a hack required to allow checks to run on a single system
        # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
        # Building it is the single-system equivalent of running "nix flake check".
        runChecks =
          nixpkgs.runCommand "run-checks"
          {
            currentSystemChecks = builtins.attrValues self.checks.${system};
          } "echo $currentSystemChecks; touch $out";
      };
    });
}
