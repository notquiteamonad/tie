{
  extras = hackage:
    {
      packages = {
        tie = ./tie.nix;
        base-noprelude = ./.stack-to-nix.cache.0;
        safe-coloured-text = ./.stack-to-nix.cache.1;
        };
      };
  resolver = "lts-17.5";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }