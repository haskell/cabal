#!/usr/bin/env bash

set -Eeuo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

# Required arguments to the script
: "${GHC_VERSION:?Need to set GHC_VERSION}"
: "${CABAL_INSTALL_VERSION:?Need to set CABAL_INSTALL_VERSION}"

# If GHC is set, extract its directory and add it to PATH
# The linux images set the GHC variable to specify the location of the GHC installation.
if [[ -n "${GHC:-}" ]]; then
  echo "GHC variable is set to: $GHC"
  export PATH="$(dirname "$GHC"):$PATH"
else
  GHC="ghc"
fi

echo "Checking toolchain versions..."

# GHC check
ghc_version_ok=false
if command -v ghc  &>/dev/null; then
  current_ghc_version=$(ghc --numeric-version)
  if [[ "$current_ghc_version" == "$GHC_VERSION" ]]; then
    ghc_version_ok=true
  else
    echo "Wrong GHC version: found $current_ghc_version, expected $GHC_VERSION"
  fi
else
  echo "GHC executable '$GHC' not found on PATH"
fi

# cabal check
cabal_version_ok=false
if command -v cabal &>/dev/null; then
  current_cabal_version=$(cabal --numeric-version)
  if [[ "$current_cabal_version" == "$CABAL_INSTALL_VERSION" ]]; then
    cabal_version_ok=true
  else
    echo "Wrong cabal version: found $current_cabal_version, expected $CABAL_INSTALL_VERSION"
  fi
else
  echo "cabal not found on PATH"
fi

# If either is wrong, install via ghcup
if ! $ghc_version_ok || ! $cabal_version_ok; then
  echo "Installing correct GHC and/or cabal via ghcup..."
  . "$CI_PROJECT_DIR/.gitlab/ghcup.sh"
fi

# Final verification (ghc and cabal must now be on PATH)
echo "Verifying installed versions..."

actual_ghc_version=$(ghc --numeric-version)
actual_cabal_version=$(cabal --numeric-version)

if [[ "$actual_ghc_version" != "$GHC_VERSION" ]]; then
  fail "Incorrect GHC version (actual: $actual_ghc_version, expected: $GHC_VERSION)"
fi

if [[ "$actual_cabal_version" != "$CABAL_INSTALL_VERSION" ]]; then
  fail "Incorrect cabal version (actual: $actual_cabal_version, expected: $CABAL_INSTALL_VERSION)"
fi

echo "Using GHC version: $actual_ghc_version"
echo "Using cabal-install version: $actual_cabal_version"

export CABAL_DIR="$CI_PROJECT_DIR/cabal"

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
        EXE_EXT=".exe"
        ;;
    *)
        EXE_EXT=""
        ;;
esac

mkdir -p "$CABAL_DIR"

# https://github.com/haskell/cabal/issues/7313#issuecomment-811851884
# and
# https://github.com/haskellari/lukko/issues/17
#
# $PLATFORM comes from CI.
if [ "$(getconf LONG_BIT)" = "32" -o "${PLATFORM:=xxx}" = "x86_64-linux-centos7" ] ; then
    echo 'constraints: lukko -ofd-locking' >> cabal.release.project.local
fi

# In February 2024, cabal started using zlib-0.7.0.0, which uses pkg-config by
# default. The GitLab CI environment doesn't (yet) supply pkg-config, and zlib
# does just fine without it on modern GHCs. That said, the CI environment
# probably *should* have pkg-config installed. See
# https://github.com/haskell/cabal/issues/9774.
echo 'constraints: zlib -pkg-config' >> cabal.release.project.local
# Furthermore, on Windows, zlib claims that libz is shipped with GHC, so it just
# uses @extra-libraries: z@ if pkg-config is False. If you are reading this
# comment, however, this didn't work. Thus we switch to using the bundled libz,
# as was done in zlib <0.7.0.0.
case "$(uname)" in
    MSYS_*|MINGW*)
        echo 'constraints: zlib +bundled-c-zlib' >> cabal.release.project.local
    ;;
esac

args=(
    --disable-profiling
    --enable-executable-stripping
    --project-file=cabal.release.project
    ${ADD_CABAL_ARGS}
)

run cabal update hackage.haskell.org,HEAD
run cabal v2-build ${args[@]} cabal-install

mkdir "$CI_PROJECT_DIR/out"
cp "$(cabal list-bin ${args[@]} cabal-install:exe:cabal)" "$CI_PROJECT_DIR/out/cabal$EXE_EXT"
cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
cd "$CI_PROJECT_DIR/out/"

# create tarball/zip
TARBALL_PREFIX="cabal-install-$("$CI_PROJECT_DIR/out/cabal" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

rm cabal plan.json
