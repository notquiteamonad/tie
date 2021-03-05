{ ghc }:
with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5df05c902cde398e056eb6271d5fe13e418db4c6.tar.gz") { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "clouded-quays-server";
  # If you change buildInputs or nativeBuildInputs, make sure you also check the Docker image
  # doesn't need any additional libraries.
  buildInputs = [ zlib ];
  shellHook = ''
    export PORT=3000
    export CQ_LOG_LEVEL=debug
    export CQ_PLAID_CLIENT_ID=5f92fc463cd69a00120499f0
    export GOOGLE_APPLICATION_CREDENTIALS=.credentials/compute-service-account-private-key.json
  '';
}
