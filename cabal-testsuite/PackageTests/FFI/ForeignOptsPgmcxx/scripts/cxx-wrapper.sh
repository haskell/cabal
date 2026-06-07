#!/bin/sh
# Wrapper around g++ that adds -D__TESTOPT_PGMCXX__=67 to every compilation.
# Used by the ForeignOptsPgmcxx test to verify that -pgmcxx selects this wrapper.
exec g++ -D__TESTOPT_PGMCXX__=67 "$@"
