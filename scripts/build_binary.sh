#!/usr/bin/env sh

function cleanUp {
  docker rm -f tie-temp &>/dev/null
}

projectDir=$(git rev-parse --show-toplevel)

# Build the executable in a docker image
docker build $projectDir -t tie:latest

trap cleanUp INT EXIT

# Create a temporary container to retrieve the executable from
docker create -it --name tie-temp tie:latest sh

# Copy the executable from the container
mkdir -p $projectDir/build
docker cp tie-temp:/root/.local/bin/tie-exe $projectDir/build/tie
