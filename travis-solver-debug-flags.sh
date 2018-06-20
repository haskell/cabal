#!/bin/sh

# Build cabal with solver debug flags enabled.
# We need to skip the tests, because debug-tracetree prints the whole solver
# tree as JSON.

cabal update
cabal install happy
cabal new-build exe:cabal --constraint "cabal-install +debug-tracetree +debug-conflict-sets"
