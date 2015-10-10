. ../common.sh

OUTPUT=`cabal manpage`

# contains visible command descriptions
echo $OUTPUT | grep -q '\.B cabal install' || die "visible command description line not found in:\n----$OUTPUT\n----"

# does not contain hidden command descriptions
echo $OUTPUT | grep -q '\.B cabal manpage' && die "hidden command description line found in:\n----$OUTPUT\n----"

exit 0
