. ../common.sh
cabal freeze --disable-benchmarks
grep -v " criterion ==" cabal.config || die "should NOT have frozen criterion"
