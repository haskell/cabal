. ./common.sh

if [[ `ghc --numeric-version` =~ "7\\." ]]; then
    cd t3199
    tmpfile=$(mktemp /tmp/cabal-t3199.XXXXXX)
    cabal sandbox init
    cabal sandbox add-source ../../../../../Cabal
    cabal install --package-db=clear --package-db=global --only-dep --dry-run > $tmpfile
    grep -q "the following would be installed" $tmpfile || die "Should've installed Cabal"
    grep -q Cabal $tmpfile || die "Should've installed Cabal"
    rm $tmpfile
fi
