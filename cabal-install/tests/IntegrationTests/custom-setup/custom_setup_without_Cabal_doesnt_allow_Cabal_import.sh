. ./common.sh
cd custom-setup-without-cabal-defaultMain

# This package has explicit setup dependencies that do not include Cabal.
# Compilation should fail because Setup.hs imports Distribution.Simple.
! cabal new-build custom-setup-without-cabal-defaultMain > output 2>&1
cat output
grep -q "\(Could not find module\|Failed to load interface for\).*Distribution\\.Simple" output \
    || die "Should not have been able to import Cabal"

grep -q "It is a member of the hidden package .*Cabal-" output \
    || die "Cabal should be available"
