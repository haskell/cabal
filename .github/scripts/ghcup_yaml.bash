#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1
ENDPOINT=$2
PR_NUMBER=$3
ENABLE_PRIVATE_RUNNERS=$4

URL="https://${ENDPOINT}/cabal-install/${PR_NUMBER}"

get_sha() {
    sha256sum "$1" | awk '{ print $1 }'
}

cat <<EOF
ghcupDownloads:
  Cabal:
    ${RELEASE}.${PR_NUMBER}:
      viTags:
        - "PR-${PR_NUMBER}"
      viArch:
        A_64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-x86_64-linux-unknown.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-x86_64-linux-unknown.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-x86_64-linux-unknown.tar.xz")
          Darwin:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-x86_64-apple-darwin.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-x86_64-apple-darwin.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-x86_64-apple-darwin.tar.xz")
          Windows:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-x86_64-mingw64.zip
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-x86_64-mingw64.zip
              dlHash: $(get_sha "cabal-install-${RELEASE}-x86_64-mingw64.zip")
EOF

if [[ "${ENABLE_PRIVATE_RUNNERS}" -eq 1 ]] ; then
cat <<EOF2
          FreeBSD:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-x86_64-portbld-freebsd.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-x86_64-portbld-freebsd.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-x86_64-portbld-freebsd.tar.xz")
EOF2
fi

cat <<EOF3
        A_32:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-i386-linux-unknown.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-i386-linux-unknown.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-i386-linux-unknown.tar.xz")
        A_ARM64:
          Darwin:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-aarch64-apple-darwin.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-aarch64-apple-darwin.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-aarch64-apple-darwin.tar.xz")
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: ${URL}/cabal-install-${RELEASE}-aarch64-linux-unknown.tar.xz
              dlOutput: cabal-install-${RELEASE}.${PR_NUMBER}-aarch64-linux-unknown.tar.xz
              dlHash: $(get_sha "cabal-install-${RELEASE}-aarch64-linux-unknown.tar.xz")
EOF3

