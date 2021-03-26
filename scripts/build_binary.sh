#!/usr/bin/env sh

projectDir=$(git rev-parse --show-toplevel)

nix-build $projectDir/nix/release/linux.nix

# Copy the executable from the container
cp ./result/bin/tie-exe $projectDir/npm/bin/tie
