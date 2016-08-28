. ./common.sh
cd custom-setup-without-cabal

# This package has explicit setup dependencies that do not include Cabal.
# new-build should try to build it, even though the cabal-version cannot be
# satisfied by an installed version of Cabal (cabal-version: >= 99999). However,
# configure should fail because Setup.hs just prints an error message and exits.
! cabal new-build custom-setup-without-cabal > output 2>&1
cat output
grep -q "My custom Setup" output \
    || die "Expected output from custom Setup"
