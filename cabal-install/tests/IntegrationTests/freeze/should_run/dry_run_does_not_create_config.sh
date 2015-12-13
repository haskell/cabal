. ../common.sh
cabal freeze --dry-run
[ ! -e cabal.config ] || die "cabal.config file should not have been created"
