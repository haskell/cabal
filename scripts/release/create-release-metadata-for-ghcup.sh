#!/bin/sh

set -eu
set -o pipefail

RELEASE=$1
VERSION=${RELEASE#cabal-install-v}
YV=$(echo "$VERSION" | sed 's/\.//g')

cd "gh-release-artifacts/cabal-${VERSION}"

BASE_URL=https://downloads.haskell.org/~cabal/cabal-install-$VERSION

get_sha() {
    sha256sum "$1" | awk '{ print $1 }'
}

print_uri_hash() {
cat <<EOF_INNER > /dev/stdout
              dlUri: ${BASE_URL}/$1
              dlHash: $(get_sha "$1")
EOF_INNER
}

cat <<EOF > /dev/stdout
    $VERSION:
      viTags:
        - Latest
      viChangeLog: https://github.com/haskell/cabal/blob/master/release-notes/cabal-install-$RELEASE.md
      # uncomment viPostInstall if the release needs a post-install message
      # viPostInstall: &cabal-${YV}-post-install |
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-unknown.tar.xz")
          Darwin:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-apple-darwin.tar.xz")
          Windows:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-mingw64.zip")
          FreeBSD:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-portbld-freebsd.tar.xz")
        A_32:
          Linux_UnknownLinux:
            unknown_versioning: &cabal-${YV}-32
$(print_uri_hash "cabal-install-$VERSION-i386-linux-unknown.tar.xz")
        A_ARM64:
          Darwin:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-aarch64-apple-darwin.tar.xz")
          Linux_UnknownLinux:
            unknown_versioning: &cabal-${YV}-arm64
$(print_uri_hash "cabal-install-$VERSION-aarch64-linux-unknown.tar.xz")
EOF
