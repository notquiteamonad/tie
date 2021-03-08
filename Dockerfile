FROM haskell:buster AS buildPhase

# Set the system charset to UTF-8
ENV LANG C.UTF-8

# Copy application config files
COPY package.yaml README.md stack.yaml stack.yaml.lock ./

# Disable building with nix
RUN sed -i '/^nix:/,$d' stack.yaml

# Enable static building
RUN sed -i '/^      - -with-rtsopts=-N$/a \ \ \ \ \ \ - -static\n\ \ \ \ \ \ - -optl-static\n\ \ \ \ \ \ - -optl-pthread' package.yaml

# Install GHC
RUN stack setup

# Build the dependencies
RUN stack build --only-dependencies

# Copy application source
COPY app app
COPY src src

# Build the app
RUN stack build --copy-bins
