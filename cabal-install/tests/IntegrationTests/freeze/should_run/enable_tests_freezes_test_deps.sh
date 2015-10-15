. ../common.sh
cabal freeze --enable-tests
grep " test-framework ==" cabal.config || die "should have frozen test-framework"
