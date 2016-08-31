. ./common.sh

cd t2755
cabal configure --enable-tests
(cabal test || true) | tee result.log
! grep "Re-configuring" result.log > /dev/null
