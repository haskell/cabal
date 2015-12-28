#!/bin/sh

find tests -type f \( -name '*.hs' -or -name '*.lhs' -or -name '*.c' -or -name '*.sh' \
    -or -name '*.cabal' -or -name '*.hsc' \) -and -not -regex ".*/dist/.*" \
    | awk '/Check.hs$|UnitTests|PackageTester|autogen|PackageTests.hs|CreatePipe/ { next } { print }' \
    | LC_COLLATE=C sort \
    | sed -e 's/^/  /' \
    > misc/source-file-list

lead='^  -- BEGIN gen-extra-source-files'
tail='^  -- END gen-extra-source-files'
# cribbed off of http://superuser.com/questions/440013/how-to-replace-part-of-a-text-file-between-markers-with-another-text-file
sed -i.bak -e "/$lead/,/$tail/{ /$lead/{p; r misc/source-file-list
              }; /$tail/p; d }" Cabal.cabal
