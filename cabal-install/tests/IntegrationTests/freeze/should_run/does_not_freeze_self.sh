. ../common.sh
cabal freeze
grep -v " my ==" cabal.config || die "should not have frozen self"
