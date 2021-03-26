#!/usr/bin/env sh

set -e

projectDir=$(git rev-parse --show-toplevel)

sudo rm -rf $projectDir/build
mkdir -p $projectDir/build/linux

# Linux Build
nix-build $projectDir/nix/release/linux.nix
cp -L ./result/bin/tie-linux $projectDir/build/linux/tie

# Windows Build
nix-build $projectDir/nix/release/windows.nix
cp -LR ./result/bin $projectDir/build/tie-windows

rm result

oldPwd=$(pwd)
cd $projectDir/build
tar cvzf tie-windows.tar.gz tie-windows
cd $oldPwd
