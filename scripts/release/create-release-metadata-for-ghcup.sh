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
          Linux_Alpine:
            '( >= 3.12 && < 3.20)': &cabal-${YV}-alpine312-64
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-alpine312.tar.xz")
            '>= 3.20':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-alpine320.tar.xz")
            unknown_versioning: *cabal-${YV}-alpine312-64
          Linux_UnknownLinux:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-unknown.tar.xz")
          Linux_Debian:
            '( >= 11 && < 12)': &cabal-${YV}-64-debian11
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-deb11.tar.xz")
            '( >= 12 && < 13)':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-deb12.tar.xz")
            unknown_versioning: *cabal-${YV}-64-debian11
          Linux_Fedora:
            '( >= 33 && < 36 )': &cabal-${YV}-64-fedora33
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-fedora33.tar.xz")
            '( >= 36 && < 38 )':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-fedora36.tar.xz")
            '>= 38':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-fedora38.tar.xz")
            unknown_versioning: *cabal-${YV}-64-fedora33
          Linux_Ubuntu:
            '(>= 20 && < 22 )': &cabal-${YV}-64-ubuntu20
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-ubuntu20.04.tar.xz")
            '(>= 22 && < 24 )':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-ubuntu22.04.tar.xz")
            '(>= 24 && < 26 )':
$(print_uri_hash "cabal-install-$VERSION-x86_64-linux-ubuntu24.04.tar.xz")
            unknown_versioning: *cabal-${YV}-64-ubuntu20
          Linux_Mint:
            unknown_versioning: *cabal-${YV}-64-ubuntu20
          Darwin:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-x86_64-darwin.tar.xz")
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
          Linux_Alpine:
            unknown_versioning: *cabal-${YV}-32
        A_ARM64:
          Darwin:
            unknown_versioning:
$(print_uri_hash "cabal-install-$VERSION-aarch64-darwin.tar.xz")
          Linux_Debian:
            '( >= 11)': &cabal-${YV}-arm64-deb
$(print_uri_hash "cabal-install-$VERSION-aarch64-linux-deb11.tar.xz")
            unknown_versioning: *cabal-${YV}-arm64-deb
          Linux_UnknownLinux:
            unknown_versioning: &cabal-${YV}-arm64
$(print_uri_hash "cabal-install-$VERSION-aarch64-linux-unknown.tar.xz")
          Linux_Alpine:
            unknown_versioning: *cabal-${YV}-arm64
EOF
