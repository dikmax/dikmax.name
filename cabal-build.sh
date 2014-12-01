#!/bin/bash

cabal sandbox init
cabal install snap-server -f openssl
cabal install --only-dependencies
cabal configure
cabal build
