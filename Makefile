.PHONY: phony
  # Adding dependency "phony" is like declaring a target as ".PHONY":
  # See https://www.gnu.org/software/make/manual/html_node/Force-Targets.html

CABALBUILD := cabal build
CABALRUN   := cabal run

# We have to avoid allow-newer.
# SEE: https://github.com/haskell/cabal/issues/6859
DOCTEST := cabal doctest --allow-newer=False

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
	cabal-validate

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

.PHONY: style-todo
style-todo: ## Configured for fourmolu, avoiding GHC parser failures
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
checks: whitespace users-guide-typos markdown-typos style lint-json  check-api ## Run all local checks; whitespace, typos, style, and lint.

# source generation: SPDX

SPDX_LICENSE_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseExceptionId.hs

# Note: the 'spdx' goal is used in .github/workflows/quick-jobs.yml.
# Any changes to this goal need to be reconciled with this workflow.
#
.PHONY: spdx
spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

SPDX_LICENSE_VERSIONS:=3.0 3.2 3.6 3.9 3.10 3.16 3.23 3.25 3.26

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
	cd Cabal-syntax && $(DOCTEST)
	cd Cabal-described && $(DOCTEST)
	cd Cabal && $(DOCTEST)
	cd cabal-install-solver && $(DOCTEST)
	cd cabal-install && $(DOCTEST)

# This is not run as part of validate.sh (we need hackage-security, which is tricky to get).
.PHONY: doctest-cli
doctest-cli :
	doctest -D__DOCTEST__ --fast cabal-install/src cabal-install-solver/src cabal-install-solver/src-assertion

.PHONY: doctest-install
doctest-install: ## Install doctest tool needed for running doctests.
	cabal install doctest --overwrite-policy=always --ignore-project --flag cabal-doctest

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

# API generation

API_GHC=9.10.1
API_PRE_FLAGS:=-w ghc-$(API_GHC) --disable-documentation --project-file=cabal.release.project
API_FLAGS:=$(API_PRE_FLAGS) --write-ghc-environment-files=always --builddir=dist-newstyle.apigen

.PHONY: generate-api
generate-api: generate-api-cabal-syntax generate-api-cabal generate-api-cabal-hooks generate-api-cabal-install-solver

.PHONY: check-api
check-api: check-api-cabal-syntax check-api-cabal check-api-cabal-hooks check-api-cabal-install-solver

.PHONY: update-api
update-api: generate-api
	mv Cabal-syntax-$(API_GHC).api Cabal-syntax/Cabal-syntax-$(API_GHC).api
	mv Cabal-$(API_GHC).api Cabal/Cabal-$(API_GHC).api
	if test -f Cabal-hooks-$(API_GHC).api; then mv Cabal-hooks-$(API_GHC).api Cabal-hooks/Cabal-hooks-$(API_GHC).api; fi
	mv cabal-install-solver-$(API_GHC).api cabal-install-solver/cabal-install-solver-$(API_GHC).api

.PHONY: check-api-cabal-syntax
check-api-cabal-syntax: generate-api-cabal-syntax
	diff -c Cabal-syntax/Cabal-syntax-$(API_GHC).api Cabal-syntax-$(API_GHC).api

.PHONY: check-api-cabal
check-api-cabal: generate-api-cabal
	diff -c Cabal/Cabal-$(API_GHC).api Cabal-$(API_GHC).api

.PHONY: check-api-cabal-hooks
check-api-cabal-hooks: generate-api-cabal-hooks
	if test -d Cabal-hooks; then diff -c Cabal-hooks/Cabal-hooks-$(API_GHC).api Cabal-hooks-$(API_GHC).api; fi

.PHONY: check-api-cabal-install-solver
check-api-cabal-install-solver: generate-api-cabal-install-solver
	diff -c cabal-install-solver/cabal-install-solver-$(API_GHC).api cabal-install-solver-$(API_GHC).api

# NB. currently print-api has no way to specify a target ghc version itself.
# The dependency on ghcup should be removed once it has one; Nix users, among
# others, won't be very happy with it.

# NB. ghc-api throws an error about a missing package db if you don't do a
# normal build before the API_FLAGS build.

.PHONY: generate-api-cabal-syntax
generate-api-cabal-syntax:
	$(CABALBUILD) Cabal-syntax $(API_PRE_FLAGS)
	$(CABALBUILD) Cabal-syntax $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal-syntax >Cabal-syntax-$(API_GHC).api

.PHONY: generate-api-cabal
generate-api-cabal:
	$(CABALBUILD) Cabal $(API_PRE_FLAGS)
	$(CABALBUILD) Cabal $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal >Cabal-$(API_GHC).api

.PHONY: generate-api-cabal-hooks
generate-api-cabal-hooks:
	if test \! -d Cabal-hooks; then \
		:; \
	else \
		$(CABALBUILD) Cabal-hooks $(API_PRE_FLAGS); \
		$(CABALBUILD) Cabal-hooks $(API_FLAGS); \
		ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal-hooks >Cabal-hooks-$(API_GHC).api; \
	fi

.PHONY: generate-api-cabal-install-solver
generate-api-cabal-install-solver:
	$(CABALBUILD) cabal-install-solver $(API_PRE_FLAGS)
	$(CABALBUILD) cabal-install-solver $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name cabal-install-solver >cabal-install-solver-$(API_GHC).api

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

bootstrap-json-%: phony
	cabal build --project-file=cabal.bootstrap.project --with-compiler=ghc-$* --dry-run cabal-install:exe:cabal
	cp dist-newstyle/cache/plan.json bootstrap/linux-$*.plan.json
	@# -v0 to avoid build output on stdout
	cd bootstrap && cabal run -v0 cabal-bootstrap-gen -- linux-$*.plan.json \
		| python3 -m json.tool > linux-$*.json

BOOTSTRAP_GHC_VERSIONS := 9.2.8 9.4.8 9.6.7 9.8.4 9.10.2 9.12.2

.PHONY: bootstrap-jsons
bootstrap-jsons: $(BOOTSTRAP_GHC_VERSIONS:%=bootstrap-json-%)

# documentation
##############################################################################

.PHONY: users-guide
users-guide: ## Build the users guide.
	$(MAKE) -C doc users-guide

ifeq ($(shell uname), Darwin)
PROCS := $(shell sysctl -n hw.logicalcpu)
else
PROCS := $(shell nproc)
endif

PHONY: help
help: ## Show the commented targets.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PHONY: help-banner
help-banner: ## Show the help banner.
	@echo "===================================================================="
	@echo "§ all                  make with no arguments also shows this banner"
	@echo "§ help                 make help will list targets with descriptions"
	@echo "===================================================================="

.PHONY: typos-install
typos-install: ## Install typos-cli for typos target using cargo
	cargo install typos-cli

GREP_EXCLUDE := grep -v -E 'dist-|cabal-testsuite|python-'
FIND_NAMED := find . -type f -name

.PHONY: users-guide-typos
users-guide-typos: ## Find typos in users guide
	cd doc && $(FIND_NAMED) '*.rst' | xargs typos

.PHONY: users-guide-fix-typos
users-guide-fix-typos: ## Fix typos in users guide
	cd doc && $(FIND_NAMED) '*.rst' | xargs typos --write-changes

.PHONY: markdown-typos
markdown-typos: ## Find typos in markdown files
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos

.PHONY: markdown-fix-typos
markdown-fix-typos: ## Fix typos in markdown files
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos --write-changes
