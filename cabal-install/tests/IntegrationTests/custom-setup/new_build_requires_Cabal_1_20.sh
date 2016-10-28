# Regression test for issue #3932

. ./common.sh

cd custom-setup-old-cabal
! cabal new-build > output 2>&1

cat output
grep -q "(issue #3932) requires >=1.20" output || die "Expect constraint failure"
