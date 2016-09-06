# Regression test for issue #3436

. ./common.sh
cabal sandbox init
cabal install ./Cabal-99998
cabal sandbox add-source Cabal-99999

# Install custom-setup, which has a setup dependency on Cabal-99999.
# cabal should build the setup script with Cabal-99999, but then
# configure should fail because Setup just prints an error message
# imported from Cabal and exits.
! cabal install custom-setup/ > output 2>&1

cat output
grep -q "This is Cabal-99999" output || die "Expected output from Cabal-99999"
