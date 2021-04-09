{ ghc }:
with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/6fc2b7ecc2a167ce6a6902d5417daf1fa5cac777.tar.gz") { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "tie";
  buildInputs = [ zlib ];
}
