# Andreas, 2023-01-26: Makefile for running the doctests.

# Needs `ghc` and `ghc-pkg` in PATH.
# Not clear how to make this multi-GHC ready: https://github.com/sol/doctest/issues/396

# doctest is GHC-specific, so we query the version.
GHC_VERSION = $(shell ghc --numeric-version)

.PHONY: doctest
doctest: install-doctest run-doctest

.PHONY: install-doctest
# doctest only works with the GHC it got installed with.
# So on ghc 9.4.4 we install doctest as doctest-9.4.4.
# `doctest --numeric-version` gives the ghc version it got installed with.
install-doctest:
	[[ "$$(doctest-$(GHC_VERSION) --numeric-version)" == "$(GHC_VERSION)" ]] \
	  || cabal install --ignore-project --overwrite-policy=always --program-suffix=-$(GHC_VERSION) doctest \
	     && [[ "$$(doctest-$(GHC_VERSION) --numeric-version)" == "$(GHC_VERSION)" ]]
                # Paranoia: test whether we really got the right version

.PHONY: run-doctest
run-doctest:
	cabal repl -w doctest-$(GHC_VERSION) --repl-options=-w

# EOF
