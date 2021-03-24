{
  extras = hackage:
    {
      packages = {
        "safe-coloured-text" = (((hackage.safe-coloured-text)."0.0.0.0").revisions)."3e614882573291a911564a0b936b2449c4855c2e315086d1337a6e6c027cf2b0";
        tie = ./tie.nix;
        base-noprelude = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-17.5";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }