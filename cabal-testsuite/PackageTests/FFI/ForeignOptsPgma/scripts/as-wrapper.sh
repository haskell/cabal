#!/bin/sh
# Wrapper around `as` that adds --d meaning_of_life_val=33 to every compilation.
# Used by the ForeignOptsPgma test to verify that -pgma selects this wrapper.
exec nasm -f macho64 "$@"
