#!/bin/sh
# Wrapper around `as` that adds --defsym meaning_of_life_val=33 to every compilation.
# Used by the ForeignOptsPgma test to verify that -pgma selects this wrapper.
exec cc -c -DMEANING_OF_LIFE_VAL=33 "$@"
