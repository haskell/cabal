. ../common.sh
cabal freeze --enable-benchmarks
grep " criterion ==" cabal.config || die "should have frozen criterion"
