. ../common.sh

OUTPUT=`cabal manpage`

# contains visible command descriptions
echo $OUTPUT | grep -qE '\.B cabal(\.exe)? install' || die "visible command description line not found in:\n----$OUTPUT\n----"

# does not contain hidden command descriptions
echo $OUTPUT | grep -qE '\.B cabal(\.exe)? manpage' && die "hidden command description line found in:\n----$OUTPUT\n----"

exit 0
