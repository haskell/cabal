. ../common.sh

# We should probably be using a .err file and should_fail,
# but this fails on windows due to the ".exe" on the cabal
# executable in the output.
cabal exec 2>&1 > /dev/null | grep "Please specify an executable to run"
