.PHONY: phony
  # Adding dependency "phony" is like declaring a target as ".PHONY":
  # See https://www.gnu.org/software/make/manual/html_node/Force-Targets.html

CABALBUILD := cabal build
CABALRUN   := cabal run

DOCTEST := cabal doctest

# default rules

.PHONY: all
all: help-banner exe lib ## Build the Cabal libraries and cabal-install executables.

.PHONY: lib
lib: ## Builds the Cabal libraries.
	$(CABALBUILD) Cabal:libs

.PHONY: exe
exe: ## Builds the cabal-install executables.
	$(CABALBUILD) cabal-install:exes

.PHONY: init
init: ## Set up git hooks and ignored revisions.
	@git config core.hooksPath .githooks
	## TODO

# NOTE: Keep this in sync with `.github/workflows/format.yml`.
FORMAT_DIRS := \
	Cabal \
	Cabal-syntax \
	cabal-install \
	cabal-testsuite/src \
	cabal-validate \
	Cabal-tests/exes

FORMAT_DIRS_TODO := \
	Cabal-QuickCheck \
	Cabal-described \
	Cabal-hooks \
	Cabal-tests \
	Cabal-tree-diff \
	bootstrap \
	buildinfo-reference-generator \
	cabal-benchmarks \
	cabal-dev-scripts \
	cabal-install-solver \
	cabal-testsuite/main \
	cabal-testsuite/static \
	solver-benchmarks

.PHONY: check
check:
	@cd Cabal && cabal check | grep -q "No errors or warnings could be found in the package."
	@cd cabal-install && cabal check | grep -q "No errors or warnings could be found in the package."
	@cd Cabal-syntax && cabal check | grep -q "No errors or warnings could be found in the package."
	@cd Cabal-hooks && cabal check | grep -q "No errors or warnings could be found in the package."
	@cd hooks-exe && cabal check | grep -q "No errors or warnings could be found in the package."
	@cd cabal-install-solver && cabal check | grep -q "No errors or warnings could be found in the package."

.PHONY: style-todo
style-todo: ## Configured for fourmolu, avoiding GHC parser failures.
	@fourmolu -q $(FORMAT_DIRS_TODO) > /dev/null

.PHONY: style
style: ## Run the code styler.
	@fourmolu -q -i $(FORMAT_DIRS)

.PHONY: style-modified
style-modified: ## Run the code styler on modified files.
	@git ls-files --modified $(FORMAT_DIRS) \
		| grep '.hs$$' | xargs -P $(PROCS) -I {} fourmolu -q -i {}

.PHONY: style-commit
style-commit: ## Run the code styler on the previous commit.
	@git diff --name-only HEAD $(COMMIT) -- $(FORMAT_DIRS) \
		| grep '.hs$$' | xargs -P $(PROCS) -I {} fourmolu -q -i {}

.PHONY: whitespace
whitespace: ## Run fix-whitespace in check mode.
	fix-whitespace --check --verbose

.PHONY: fix-whitespace
fix-whitespace: ## Run fix-whitespace in fix mode.
	fix-whitespace --verbose

.PHONY: lint
lint: ## Run HLint.
	hlint -j .

.PHONY: lint-json
lint-json: ## Run HLint in JSON mode.
	hlint -j --json -- .

# local checks

.PHONY: checks
checks: whitespace users-guide-typos markdown-typos style lint-json  ## Run all local checks: whitespace, typos, style, and lint.

# source generation: SPDX

SPDX_LICENSE_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseExceptionId.hs

# Note: the 'spdx' goal is used in .github/workflows/quick-jobs.yml.
# Any changes to this goal need to be reconciled with this workflow.
#
.PHONY: spdx
spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

SPDX_LICENSE_VERSIONS:=3.0 3.2 3.6 3.9 3.10 3.16 3.23 3.25 3.26 3.28

$(SPDX_LICENSE_HS) : templates/SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project gen-spdx -- templates/SPDX.LicenseId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/licenses-%.json) $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : templates/SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project gen-spdx-exc -- templates/SPDX.LicenseExceptionId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/exceptions-%.json) $(SPDX_EXCEPTION_HS)

# source generation: templates

TEMPLATE_MACROS:=Cabal/src/Distribution/Simple/Build/Macros/Z.hs
TEMPLATE_PATHS:=Cabal/src/Distribution/Simple/Build/PathsModule/Z.hs

# Note: the 'templates' goal is used in .github/workflows/quick-jobs.yml.
# Any changes to this goal need to be reconciled with this workflow.
#
.PHONY: templates
templates : $(TEMPLATE_MACROS) $(TEMPLATE_PATHS)

$(TEMPLATE_MACROS) : templates/cabal_macros.template.h cabal-dev-scripts/src/GenCabalMacros.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project gen-cabal-macros -- $< $@

$(TEMPLATE_PATHS) : templates/Paths_pkg.template.hs cabal-dev-scripts/src/GenPathsModule.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project gen-paths-module -- $< $@

# generated docs
# Use cabal build before cabal run to avoid output of the build on stdout when running
doc/buildinfo-fields-reference.rst : \
  $(wildcard Cabal-syntax/src/*/*.hs Cabal-syntax/src/*/*/*.hs Cabal-syntax/src/*/*/*/*.hs) \
  $(wildcard Cabal-described/src/Distribution/Described.hs Cabal-described/src/Distribution/Utils/*.hs) \
  buildinfo-reference-generator/src/Main.hs \
  buildinfo-reference-generator/template.zinza
	cabal build buildinfo-reference-generator
	cabal run buildinfo-reference-generator buildinfo-reference-generator/template.zinza | tee $@
	git diff --exit-code $@

.PHONY: analyse-imports
analyse-imports :
	find Cabal-syntax/src Cabal/src cabal-install/src -type f -name '*.hs' | xargs cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project analyse-imports --

# ghcid

.PHONY: ghcid-lib
ghcid-lib: ## Run ghcid for the Cabal library.
	ghcid -c 'cabal repl Cabal'

.PHONY: ghcid-cli
ghcid-cli: ## Run ghcid for the cabal-install executable.
	ghcid -c 'cabal repl cabal-install'

.PHONY: doctest
doctest: ## Run doctests.
	cabal --numeric-version
	cd Cabal-syntax && $(DOCTEST)
	cd Cabal-described && $(DOCTEST)
	cd Cabal && $(DOCTEST)
	cd cabal-install-solver && $(DOCTEST)
	cd cabal-install && $(DOCTEST)

# If we pin and periodically bump the version of doctest, we get more
# reproducible testing. Initially we pinned to doctest-0.25.0 to avoid failures
# with doctest-0.24.3 but `cabal install doctest` depends on index state that
# itself gets bumped when `cabal update --ignore-project` is run.
# SEE: https://github.com/haskell/cabal/issues/11493#issuecomment-4615438425
.PHONY: doctest-install
doctest-install: ## Install doctest tool needed for running doctests.
	cabal install doctest-0.25.0 --overwrite-policy=always --ignore-project --flag cabal-doctest

# tests

.PHONY: check-tests
check-tests :
	$(CABALRUN) check-tests -- --cwd Cabal-tests ${TEST}

.PHONY: parser-tests
parser-tests :
	$(CABALRUN) parser-tests -- --cwd Cabal-tests ${TEST}

.PHONY: parser-tests-accept
parser-tests-accept :
	$(CABALRUN) parser-tests -- --cwd Cabal-tests --accept ${TEST}

.PHONY: custom-setup-tests
custom-setup-tests :
	$(CABALRUN) custom-setup-tests --

.PHONY: hackage-parsec-tests
hackage-parsec-tests :
	$(CABALRUN) hackage-tests -- parsec +RTS -s -qg -I0 -A64M -N${THREADS} -RTS ${TEST}

.PHONY: hackage-roundtrip-tests
hackage-roundtrip-tests :
	$(CABALRUN) hackage-tests -- roundtrip +RTS -s -qg -I0 -A64M -N${THREADS} -RTS ${TEST}

.PHONY: cabal-install-test
cabal-install-test:
	$(CABALBUILD) -j3 cabal-tests cabal
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal list-bin cabal-tests` --with-cabal=`cabal list-bin cabal` --hide-successes -j3 ${TEST}

# hackage-benchmarks (solver)

.PHONY: hackage-benchmarks-run
hackage-benchmarks-run:
	$(CABALBUILD) -j3 hackage-benchmark cabal
	rm -rf .ghc.environment.*
	$$(cabal list-bin hackage-benchmark) --cabal1=cabal --cabal2=$$(cabal list-bin cabal) --packages="hakyll servant-auth-server" --print-trials --concurrently


# This doesn't run build, as you first need to test with cabal-install-test :)
.PHONY: cabal-install-test-accept
cabal-install-test-accept:
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal list-bin cabal-tests` --with-cabal=`cabal list-bin cabal` --hide-successes -j3 --accept ${TEST}

# Docker validation

# Use this carefully, on big machine you can say
#
#   make validate-via-docker-all -j4 -O
#
.PHONY: validate-via-docker-all
validate-via-docker-all : validate-via-docker-8.2.2
validate-via-docker-all : validate-via-docker-8.4.4
validate-via-docker-all : validate-via-docker-8.8.4
validate-via-docker-all : validate-via-docker-8.10.4

.PHONY: validate-dockerfiles
validate-dockerfiles : .docker/validate-8.10.4.dockerfile
validate-dockerfiles : .docker/validate-8.8.4.dockerfile
validate-dockerfiles : .docker/validate-8.4.4.dockerfile
validate-dockerfiles : .docker/validate-8.2.2.dockerfile

.docker/validate-%.dockerfile : .docker/validate.dockerfile.zinza cabal-dev-scripts/src/GenValidateDockerfile.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.meta.project gen-validate-dockerfile -- $* $< $@

# This is good idea anyway
# and we have a test relying on this limit being sufficiently small
DOCKERARGS:=--ulimit nofile=1024:1024

.PHONY: validate-via-docker-8.2.2
validate-via-docker-8.2.2:
	docker build $(DOCKERARGS) -t cabal-validate:8.2.2 -f .docker/validate-8.2.2.dockerfile .

.PHONY: validate-via-docker-8.4.4
validate-via-docker-8.4.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.4.4 -f .docker/validate-8.4.4.dockerfile .

.PHONY: validate-via-docker-8.8.4
validate-via-docker-8.8.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.8.4 -f .docker/validate-8.8.4.dockerfile .

.PHONY: validate-via-docker-8.10.4
validate-via-docker-8.10.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.10.4 -f .docker/validate-8.10.4.dockerfile .

.PHONY: validate-via-docker-old
validate-via-docker-old:
	docker build $(DOCKERARGS) -t cabal-validate:older -f .docker/validate-old.dockerfile .

# tags
.PHONY : tags
tags: ## Generate editor tags, vim ctags and emacs etags.
	hasktags -b Cabal-syntax/src Cabal/src Cabal-described/src cabal-install/src cabal-testsuite/src

# bootstrapping
##############################################################################

BOOT_GHCUP := $(shell ghcup --version 2>/dev/null)
BOOT_NIX := $(shell nix-shell --version 2>/dev/null)
CABALCONF := $(shell if test -f $$HOME/.config/cabal/config; then echo CABAL_CONFIG=$$HOME/.config/cabal/config; fi)

bootstrap-json-%: phony
	ghcup install ghc "$*" --no-verbose --no-guess-version
	$(MAKE) "bootstrap-subr-$*"

# "subroutine", make style
bootstrap-subr-%: phony
	rm -rf dist-bootstrap
	$(CABALBUILD) --distdir=dist-bootstrap --project-file=cabal.bootstrap.project --with-compiler=ghc-$* --dry-run \
		cabal-install:exe:cabal
	cp dist-bootstrap/cache/plan.json bootstrap/linux-$*.plan.json
	@# -v0 to avoid build output on stdout
	cd bootstrap && $(CABALCONF) $(CABALRUN) -v0 cabal-bootstrap-gen -- linux-$*.plan.json \
		| python3 -m json.tool > linux-$*.json

BOOTSTRAP_GHC_VERSIONS := 9.6.7 9.8.4 9.10.3 9.12.4

.PHONY: bootstrap-jsons
bootstrap-jsons: ## Generate bootstrap JSONs for Linux (autodetects method).
ifeq ($(BOOT_GHCUP), )
ifeq ($(BOOT_NIX), )
	@echo If you do not have the following ghc versions, you will need to install them manually.
	@echo They can be obtained from https://downloads.haskell.org/ghc.
	@echo "   " $(BOOTSTRAP_GHC_VERSIONS)
	$(MAKE) bootstrap-jsons-no-dl
else
	$(MAKE) bootstrap-jsons-nix
endif
else
	$(MAKE) bootstrap-jsons-ghcup
endif

.PHONY: bootstrap-jsons-no-dl
bootstrap-jsons-no-dl: $(BOOTSTRAP_GHC_VERSIONS:%=bootstrap-subr-%) ## Generate bootstrap JSONs for Linux using existing ghcs.

.PHONY: bootstrap-jsons-nix
bootstrap-jsons-nix: ## Generate bootstrap JSONs for Linux using Nix-provided ghcs.
	cd ./bootstrap && $(CABALCONF) ./generate_bootstrap_plans

.PHONY: bootstrap-jsons-ghcup
bootstrap-jsons-ghcup: $(BOOTSTRAP_GHC_VERSIONS:%=bootstrap-json-%) ## Generate bootstrap JSONs for Linux using ghcup.

# this lets the Nix bootstrap script use the Makefile's definitions instead of hardcoding them itself
.PHONY: bootstrap-jsons-nix-helper
bootstrap-jsons-nix-helper:
	@echo "$(BOOTSTRAP_GHC_VERSIONS)"

# documentation
##############################################################################

.PHONY: users-guide
users-guide: ## Build the users guide.
	$(MAKE) -C doc users-guide

# Haddock for one module of a library in this repository, typechecked against
# the built in-place library instead of re-haddocking the whole package. The
# library must already be built, with `cabal build $(PKG):lib:$(PKG)`. The
# language and extensions below must cover every library stanza in the
# repository; they all use GHC2021.
#
# The module may import its package's other-modules, which the registered
# library hides, so the recipe registers a copy of the package in a temporary
# package db with every module exposed.
#
#   make haddock-module MODULE=Distribution.Client.Cmd.UI
#   make haddock-module PKG=Cabal-syntax MODULE=Distribution.Types.Version
#
HADDOCK_MODULE_OUT ?= dist-newstyle/haddock-module
HADDOCK_MODULE_GHC = $(shell ghc --numeric-version)
HADDOCK_MODULE_EXTS := -XGHC2021 -XTypeOperators -XNoImportQualifiedPost
PKG ?= cabal-install

.PHONY: haddock-module
haddock-module: ## Haddock one module: MODULE=Distribution.Client.Cmd.UI [PKG=cabal-install]
	@test -n "$(MODULE)" || { echo "usage: make haddock-module MODULE=Distribution.Client.Cmd.UI [PKG=cabal-install]"; exit 1; }
	@pkgdb=dist-newstyle/packagedb/ghc-$(HADDOCK_MODULE_GHC) && \
	conf=$$(ls $$pkgdb/$(PKG)-[0-9]*-inplace.conf) && \
	deps=$$(awk '/^depends:/{f=1; sub(/^depends:/,"")} f && /^[a-z-]+:/ && !/^depends:/{f=0} f{print}' $$conf \
		| tr ' ' '\n' | grep . | sed 's/^/--optghc=-package-id=/') && \
	dbs=$$(for db in $$(cabal path -v0 --store-dir)/ghc-$(HADDOCK_MODULE_GHC)-*/package.db; do echo "--optghc=-package-db=$$db"; done) && \
	macros=$$(ls dist-newstyle/build/*/ghc-$(HADDOCK_MODULE_GHC)/$(PKG)-[0-9]*/build/autogen/cabal_macros.h) && \
	src=$$(ls $(PKG)/src*/$(subst .,/,$(MODULE)).hs) && \
	tmpdb=$$(mktemp -d) && trap 'rm -rf "$$tmpdb"' EXIT && \
	cp $$pkgdb/*.conf $$tmpdb && \
	awk 'NR==FNR { if (/^hidden-modules:/) { h=1; sub(/^hidden-modules:/, "") } else if (h && /^[A-Za-z-]+:/) h=0; if (h) for (i=1; i<=NF; i++) hid = hid "    " $$i ",\n"; next } \
		/^hidden-modules:/ { skip=1; next } skip && /^[A-Za-z-]+:/ { skip=0 } skip { next } \
		/^exposed-modules:/ { print; printf "%s", hid; next } { print }' $$conf $$conf > $$tmpdb/$$(basename $$conf) && \
	ghc-pkg recache --package-db=$$tmpdb && \
	haddock --html -o $(HADDOCK_MODULE_OUT) \
		$(addprefix --optghc=,$(HADDOCK_MODULE_EXTS)) \
		--optghc=-optP-include --optghc=-optP$$macros \
		--optghc=-hide-all-packages $$dbs \
		--optghc=-package-db=$$tmpdb \
		$$deps --optghc=-package-id=$$(basename $$conf .conf) \
		$$src
	@echo "$(HADDOCK_MODULE_OUT)/$(subst .,-,$(MODULE)).html"

ifeq ($(shell uname), Darwin)
PROCS := $(shell sysctl -n hw.logicalcpu)
else
PROCS := $(shell nproc)
endif

.PHONY: help
help: ## Show the commented targets.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help-banner
help-banner: ## Show the help banner.
	@echo "===================================================================="
	@echo "§ all                  make with no arguments also shows this banner"
	@echo "§ help                 make help will list targets with descriptions"
	@echo "===================================================================="

.PHONY: typos-install
typos-install: ## Install typos-cli for typos target using cargo.
	cargo install typos-cli

GREP_EXCLUDE := grep -v -E 'dist-|cabal-testsuite|python-'
FIND_NAMED := find . -type f -name

.PHONY: hs-typos
hs-typos: ## Find typos in Haskell source files: .hs, .cabal, etc.
	typos --config .typos-srcs.toml --force-exclude

.PHONY: hs-fix-typos
hs-fix-typos: ## Fix typos in Haskell source files: .hs, .cabal, etc.
	typos --config .typos-srcs.toml --write-changes --force-exclude

.PHONY: users-guide-typos
users-guide-typos: ## Find typos in users guide.
	cd doc && $(FIND_NAMED) '*.rst' | xargs typos --config ../.typos-docs.toml --force-exclude

.PHONY: users-guide-fix-typos
users-guide-fix-typos: ## Fix typos in users guide.
	cd doc && $(FIND_NAMED) '*.rst' | xargs typos --config ../.typos-docs.toml --write-changes --force-exclude

.PHONY: markdown-typos
markdown-typos: ## Find typos in markdown files.
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos --config .typos-docs.toml --force-exclude

.PHONY: markdown-fix-typos
markdown-fix-typos: ## Fix typos in markdown files.
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos --config .typos-docs.toml --write-changes --force-exclude
