. ../common.sh
# TODO: Test this against a package installed in the sandbox but not
# depended upon.
cabal freeze
grep -v "exceptions ==" cabal.config || die "should not have frozen exceptions"
