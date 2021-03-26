#!/usr/bin/env sh

projectDir=$(git rev-parse --show-toplevel)

rm -rf $projectDir/build
mkdir -p $projectDir/build/linux

# Linux Build
nix-build $projectDir/nix/release/linux.nix
cp -L ./result/bin/tie-linux $projectDir/build/linux/tie

# Windows Build
nix-build $projectDir/nix/release/windows.nix
cp -LR ./result/bin $projectDir/build/windows

rm result

