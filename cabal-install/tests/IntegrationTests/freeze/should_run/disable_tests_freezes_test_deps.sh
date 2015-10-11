. ../common.sh
cabal freeze --disable-tests
grep -v " test-framework ==" cabal.config || die "should NOT have frozen test-framework"
