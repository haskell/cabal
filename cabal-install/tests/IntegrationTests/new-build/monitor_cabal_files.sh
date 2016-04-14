. ./common.sh
cd monitor_cabal_files
cp q/q-broken.cabal.in q/q.cabal
echo "Run 1" | awk '{print;print > "/dev/stderr"}'
! cabal new-build q
cp q/q-fixed.cabal.in q/q.cabal
echo "Run 2" | awk '{print;print > "/dev/stderr"}'
cabal new-build q
