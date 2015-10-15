. ../common.sh
cabal freeze
grep " ghc-prim ==" cabal.config || die "'ghc-prim' should have been frozen"
