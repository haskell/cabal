. ../common.sh
cabal freeze
grep " base ==" cabal.config || die "'base' should have been frozen"
