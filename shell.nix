{ ghc }:
with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5df05c902cde398e056eb6271d5fe13e418db4c6.tar.gz") { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "tie";
  buildInputs = [ zlib ];
}
