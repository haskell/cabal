. ../common.sh
# TODO: solver should find solution without extra flags too
cabal freeze --enable-benchmarks --reorder-goals --max-backjumps=-1
grep " criterion ==" cabal.config || die "should have frozen criterion"
