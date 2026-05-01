#!/bin/sh
# Wrapper around cc that adds -D__TESTOPT_PGMC__=66 to every compilation.
# Used by the ForeignOptsPgmc test to verify that -pgmc selects this wrapper.
exec cc -D__TESTOPT_PGMC__=66 "$@"
