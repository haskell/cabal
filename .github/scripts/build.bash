set -eux

uname -a
uname -p
uname
pwd
env

if [ "${RUNNER_OS}" = Windows ] ; then
    ext=".exe"
else
    ext=""
fi

ghcup --no-verbose install ghc --set --install-targets "${GHC_TARGETS}" "${GHC_VERSION}"

cabal update
cabal user-config diff
cabal user-config init -f
"ghc-${GHC_VERSION}" --info
"ghc" --info

# shellcheck disable=SC2206
args=(
    -w "ghc-$GHC_VERSION"
    --disable-profiling
    --enable-executable-stripping
    --project-file=cabal.release.project
    ${ADD_CABAL_ARGS}
)

cabal v2-build "${args[@]}" cabal-install

mkdir -p "out"
# shellcheck disable=SC2154
cp "$(cabal list-bin "${args[@]}" cabal-install:exe:cabal)" "out/cabal$ext"
cp dist-newstyle/cache/plan.json "out/plan.json"
cd "out/"

# create tarball/zip
TARBALL_PREFIX="cabal-install-$("./cabal" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${ARTIFACT}.${TARBALL_EXT}" "cabal${ext}" plan.json
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${ARTIFACT}.${TARBALL_EXT}" "cabal${ext}" plan.json
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

rm "cabal${ext}" plan.json

