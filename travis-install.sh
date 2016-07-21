#!/bin/sh
set -ex

travis_retry () {
    $*  || (sleep 1 && $*) || (sleep 2 && $*)
}

if [ "$GHCVER" = "none" ]; then
    exit 0
fi

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    travis_retry sudo add-apt-repository -y ppa:hvr/ghc
    travis_retry sudo apt-get update
    travis_retry sudo apt-get install --force-yes cabal-install-1.24 happy-1.19.5 ghc-$GHCVER-prof ghc-$GHCVER-dyn
    if [ "$TEST_OLDER" == "YES" ]; then travis_retry sudo apt-get install --force-yes ghc-7.0.4-prof ghc-7.0.4-dyn ghc-7.2.2-prof ghc-7.2.2-dyn; fi

elif [ "$TRAVIS_OS_NAME" = "osx" ]; then

    case $GHCVER in
        8.0.1)
            GHCURL=http://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-x86_64-apple-darwin.tar.xz;
            GHCXZ=YES
            ;;
        7.10.3)
            GHCURL=http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3b-x86_64-apple-darwin.tar.xz
            GHCXZ=YES
            ;;
        7.8.4)
            GHCURL=https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-apple-darwin.tar.xz
            GHCXZ=YES
            ;;
        7.6.3)
            GHCURL=https://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-apple-darwin.tar.bz2
            ;;
        7.4.2)
            GHCURL=https://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-x86_64-apple-darwin.tar.bz2
            ;;
        *)
            echo "Unknown GHC: $GHCVER"
            false
            ;;
    esac

    travis_retry curl -OL $GHCURL
    if [ "$GHCXZ" = "YES" ]; then
        tar -xJf ghc-*.tar.*;
    else
        tar -xjf ghc-*.tar.*;
    fi

    cd ghc-*;
    ./configure --prefix=$HOME/.ghc-install/$GHCVER
    make install;
    cd ..;

    travis_retry curl -L https://www.haskell.org/cabal/release/cabal-install-1.24.0.0/cabal-install-1.24.0.0-x86_64-apple-darwin-yosemite.tar.gz -o cabal-install.tar.gz
    TAR=$PWD/cabal-install.tar.gz
    mkdir "${HOME}/bin"
    (cd "${HOME}/bin" && tar -xzf "$TAR")
    "${HOME}/bin/cabal" --version

else
    echo "Not linux or osx: $TRAVIS_OS_NAME"
    false
fi

git version
