#!/usr/bin/env bash

# This script, when passed the cabal release number as the first and only argument
# generates the metadata in the correct format to be useable as is by GHCup
# for eg:-
# $ create-release-metadata-for-ghcup.sh 3.10.2.0 or "3.10.2.0"

# Note:- Please run ./download-cabal-install-release-binaries.sh before running this script.
set -eu
set -o pipefail

RELEASE=$1
## FixMe:// What dir to use here?

if [ -d "binary-downloads/cabal-install-${RELEASE}-binaries" ]; then
    echo "binary downloads folder for release ${RELEASE} found, starting generating GHCup metadata..."
else
    echo "The binary downloads for release ${RELEASE} not found."
    echo "Please run the script to download them first."
fi

cd "binary-downloads/cabal-install-${RELEASE}-binaries"

cat <<EOF > /dev/stdout
    $RELEASE:
      viTags:
        - Latest
      viChangeLog: https://github.com/haskell/cabal/blob/master/release-notes/cabal-install-$RELEASE.md
      viPostInstall: *cabal-${RELEASE//./}-post-install
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning: &cabal-${RELEASE//./}-64
            dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-alpine3_12.tar.xz
            dlSubdir: cabal-install-$RELEASE
            dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-alpine3_12.tar.xz" | awk '{ print $1 }')
          Linux_Alpine:
            unknown_versioning: &cabal-${RELEASE//./}-64
          Linux_CentOS:
            unknown_versioning: &cabal-${RELEASE//./}-64-centos7
            dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-centos7.tar.xz
            dlSubdir: cabal-install-$RELEASE
            dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-centos7.tar.xz" | awk '{ print $1 }')
          Linux_Debian:
            ' ( >= 9 && < 10)': &cabal-${RELEASE//./}-64-debian
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-deb9.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-deb9.tar.xz" | awk '{ print $1 }')
            ' ( == 10 && < 11)':
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-deb10.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-deb10.tar.xz" | awk '{ print $1 }')
            ' ( >= 11)':
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-deb11.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-deb11.tar.xz" | awk '{ print $1 }')
            unknown_versioning: &cabal-${RELEASE//./}-64-debian
          Linux_Fedora:
            '>= 33':
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-fedora33.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-fedora33.tar.xz" | awk '{ print $1 }')
            unknown_versioning: &cabal-${RELEASE//./}-64-centos7
          Linux_Ubuntu:
            '< 20': &cabal-${RELEASE//./}-64-ubuntu18
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-ubuntu18_04.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-ubuntu18_04.tar.xz" | awk '{ print $1 }')
            '>= 20': &cabal-${RELEASE//./}-64-ubuntu20
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-linux-ubuntu20_04.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-linux-ubuntu20_04.tar.xz" | awk '{ print $1 }')
            unknown_versioning: *cabal-${RELEASE//./}-64-ubuntu18
          Linux_Mint:
            '< 20': *cabal-${RELEASE//./}-64-ubuntu18
            '>= 20': *cabal-${RELEASE//./}-64-ubuntu20
            unknown_versioning: *cabal-${RELEASE//./}-64-ubuntu18
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-darwin.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-darwin.tar.xz" | awk '{ print $1 }')
          Windows:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-windows.zip
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-windows.zip" | awk '{ print $1 }')
          FreeBSD:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-x86_64-freebsd.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-x86_64-freebsd.tar.xz" | awk '{ print $1 }')
        A_32:
          Linux_UnknownLinux:
            unknown_versioning: &cabal-${RELEASE//./}-32
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-i386-linux-alpine3_12.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-i386-linux-alpine3_12.tar.xz" | awk '{ print $1 }')
          Linux_Alpine:
            unknown_versioning: *cabal-${RELEASE//./}-32
          Linux_Debian:
            '( >= 9 )':
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-i386-linux-deb9.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-i386-linux-deb9.tar.xz" | awk '{ print $1 }')
            unknown_versioning: *cabal-${RELEASE//./}-32
        A_ARM64:
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-aarch64-darwin.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-aarch64-darwin.tar.xz" | awk '{ print $1 }')
          Linux_Debian:
            '( >= 10 && < 11)': &cabal-31020-arm64
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-aarch64-linux-deb10.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-aarch64-linux-deb10.tar.xz" | awk '{ print $1 }')
            '( >= 11)':
              dlUri: https://downloads.haskell.org/~cabal/cabal-install-$RELEASE/cabal-install-$RELEASE-aarch64-linux-deb11.tar.xz
              dlSubdir: cabal-install-$RELEASE
              dlHash: $(sha256sum "cabal-install-$RELEASE-aarch64-linux-deb11.tar.xz" | awk '{ print $1 }')
            unknown_versioning: *cabal-${RELEASE//./}-arm64
          Linux_UnknownLinux:
            unknown_versioning: *cabal-${RELEASE//./}-arm64
EOF
