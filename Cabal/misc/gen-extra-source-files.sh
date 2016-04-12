#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "Error: too few arguments!"
    echo "Usage: $0 FILE"
    exit 1
fi

set -ex

git ls-files tests \
    | awk '/\.(hs|lhs|c|sh|cabal|hsc|err|out|in)$|ghc/ { print } { next }' \
    | awk '/Check.hs$|UnitTests|PackageTester|autogen|register.sh|PackageTests.hs|IntegrationTests.hs|CreatePipe|^tests\/Test/ { next } { print }' \
    | LC_ALL=C sort \
    | sed -e 's/^/  /' \
    > source-file-list

lead='^  -- BEGIN gen-extra-source-files'
tail='^  -- END gen-extra-source-files'
# cribbed off of http://superuser.com/questions/440013/how-to-replace-part-of-a-text-file-between-markers-with-another-text-file
sed -i.bak -e "/$lead/,/$tail/{ /$lead/{p; r source-file-list
              }; /$tail/p; d }" $1
