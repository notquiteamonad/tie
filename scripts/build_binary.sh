#!/usr/bin/env sh

function cleanUp {
  docker rm -f tie-temp &>/dev/null
}

# Build the executable in a docker image
docker build $(git rev-parse --show-toplevel) -t tie:latest

trap cleanUp INT EXIT

# Create a temporary container to retrieve the executable from
docker create -it --name tie-temp tie:latest sh

# Copy the executable from the container
mkdir -p build
docker cp tie-temp:/root/.local/bin/tie-exe build/tie
