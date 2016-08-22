. ./common.sh
if [ "x$(uname)" != "xLinux" ]; then
    exit
fi
# Older GHCs don't report exit via signal adequately
require_ghc_ge 708
cd segfault
! cabal new-build 2> log
cat log
grep SIGSEGV log
