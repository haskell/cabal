#! /bin/sh

find tests -type f \( -name '*.hs' -or -name '*.c' -or -name '*.sh' \
    -or -name '*.cabal' -or -name '*.hsc' \) -and -not -regex ".*/dist/.*" \
    | awk '/Check.hs$|UnitTests|PackageTester|autogen|PackageTests.hs|CreatePipe/ { next } { print }'
