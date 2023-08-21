.PHONY : all lexer sdpx lib exe doctest
.PHONY : phony

CABALBUILD := cabal build
CABALRUN   := cabal run

# default rules

all : exe lib

lib : $(LEXER_HS)
	$(CABALBUILD) Cabal:libs

exe : $(LEXER_HS)
	$(CABALBUILD) cabal-install:exes

init: ## Set up git hooks and ignored revisions
	@git config core.hooksPath .githooks
	## TODO

style: ## Run the code styler
	@fourmolu -q -i Cabal Cabal-syntax cabal-install

style-modified: ## Run the code styler on modified files
	@git ls-files --modified Cabal Cabal-syntax cabal-install \
		| grep '.hs$$' | xargs -P $(PROCS) -I {} fourmolu -q -i {}

# source generation: SPDX

SPDX_LICENSE_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal-syntax/src/Distribution/SPDX/LicenseExceptionId.hs

spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

SPDX_LICENSE_VERSIONS:=3.0 3.2 3.6 3.9 3.10 3.16

$(SPDX_LICENSE_HS) : templates/SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx -- templates/SPDX.LicenseId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/licenses-%.json) $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : templates/SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx-exc -- templates/SPDX.LicenseExceptionId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/exceptions-%.json) $(SPDX_EXCEPTION_HS)

# source generation: templates

TEMPLATE_MACROS:=Cabal/src/Distribution/Simple/Build/Macros/Z.hs
TEMPLATE_PATHS:=Cabal/src/Distribution/Simple/Build/PathsModule/Z.hs

templates : phony $(TEMPLATE_MACROS) $(TEMPLATE_PATHS)

$(TEMPLATE_MACROS) : templates/cabal_macros.template.h cabal-dev-scripts/src/GenCabalMacros.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-cabal-macros -- $< $@

$(TEMPLATE_PATHS) : templates/Paths_pkg.template.hs cabal-dev-scripts/src/GenPathsModule.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-paths-module -- $< $@

# generated docs

buildinfo-fields-reference : phony
	cabal build --builddir=dist-newstyle-bi --project-file=cabal.project.buildinfo buildinfo-reference-generator
	$$(cabal list-bin --builddir=dist-newstyle-bi buildinfo-reference-generator) buildinfo-reference-generator/template.zinza | tee $@

# analyse-imports
analyse-imports : phony
	find Cabal-syntax/src Cabal/src cabal-install/src -type f -name '*.hs' | xargs cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta analyse-imports --

# ghcid

ghcid-lib :
	ghcid -c 'cabal repl Cabal'

ghcid-cli :
	ghcid -c 'cabal repl cabal-install'

# Artem, 2023-02-03, https://github.com/haskell/cabal/issues/8504
# The new and prefered way to call the doctest tool (as of now) is based on cabal repl --with-ghc=doctest.
# The call below reflects the current documentation of the doctest tool except one caveat,
# which is https://github.com/haskell/cabal/issues/6859, i.e. we have to hide allow-newer in our project
# file from cabal/doctest. This is easy: we just select a project file with no allow-newer (e.g. cabal.project.libonly).
#
# TODO: Cabal-described should be added here but its doctests currently broken, see:
#       https://github.com/haskell/cabal/issues/8734
#       Just as well, cabal-install(-solver) doctests (the target below) bitrotted and need some care.
doctest :
	cabal repl --with-ghc=doctest --build-depends=QuickCheck --build-depends=template-haskell --repl-options="-w" --project-file="cabal.project.validate" Cabal-syntax
	cabal repl --with-ghc=doctest --build-depends=QuickCheck --build-depends=template-haskell --repl-options="-w" --project-file="cabal.project.validate" Cabal


# This is not run as part of validate.sh (we need hackage-security, which is tricky to get).
doctest-cli :
	doctest -D__DOCTEST__ --fast cabal-install/src cabal-install-solver/src cabal-install-solver/src-assertion

doctest-install:
	cabal install doctest --overwrite-policy=always --ignore-project

# tests

check-tests :
	$(CABALRUN) check-tests -- --cwd Cabal-tests ${TEST}

parser-tests :
	$(CABALRUN) parser-tests -- --cwd Cabal-tests ${TEST}

parser-tests-accept :
	$(CABALRUN) parser-tests -- --cwd Cabal-tests --accept ${TEST}

custom-setup-tests :
	$(CABALRUN) custom-setup-tests --

hackage-parsec-tests :
	$(CABALRUN) hackage-tests -- parsec +RTS -s -qg -I0 -A64M -N${THREADS} -RTS ${TEST}

hackage-roundtrip-tests :
	$(CABALRUN) hackage-tests -- roundtrip +RTS -s -qg -I0 -A64M -N${THREADS} -RTS ${TEST}

cabal-install-test:
	$(CABALBUILD) -j3 cabal-tests cabal
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal list-bin cabal-tests` --with-cabal=`cabal list-bin cabal` --hide-successes -j3 ${TEST}

# hackage-benchmarks (solver)

hackage-benchmarks-run:
	$(CABALBUILD) -j3 hackage-benchmark cabal
	rm -rf .ghc.environment.*
	$$(cabal list-bin hackage-benchmark) --cabal1=cabal --cabal2=$$(cabal list-bin cabal) --packages="hakyll servant-auth-server" --print-trials --concurrently


# This doesn't run build, as you first need to test with cabal-install-test :)
cabal-install-test-accept:
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal list-bin cabal-tests` --with-cabal=`cabal list-bin cabal` --hide-successes -j3 --accept ${TEST}

# Docker validation

# Use this carefully, on big machine you can say
#
#   make validate-via-docker-all -j4 -O
#
validate-via-docker-all : validate-via-docker-8.2.2
validate-via-docker-all : validate-via-docker-8.4.4
validate-via-docker-all : validate-via-docker-8.6.5
validate-via-docker-all : validate-via-docker-8.8.4
validate-via-docker-all : validate-via-docker-8.10.4

validate-dockerfiles : .docker/validate-8.10.4.dockerfile
validate-dockerfiles : .docker/validate-8.8.4.dockerfile
validate-dockerfiles : .docker/validate-8.6.5.dockerfile
validate-dockerfiles : .docker/validate-8.4.4.dockerfile
validate-dockerfiles : .docker/validate-8.2.2.dockerfile

.docker/validate-%.dockerfile : .docker/validate.dockerfile.zinza cabal-dev-scripts/src/GenValidateDockerfile.hs
	cabal run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-validate-dockerfile -- $* $< $@

# This is good idea anyway
# and we have a test relying on this limit being sufficiently small
DOCKERARGS:=--ulimit nofile=1024:1024

validate-via-docker-8.2.2:
	docker build $(DOCKERARGS) -t cabal-validate:8.2.2 -f .docker/validate-8.2.2.dockerfile .

validate-via-docker-8.4.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.4.4 -f .docker/validate-8.4.4.dockerfile .

validate-via-docker-8.6.5:
	docker build $(DOCKERARGS) -t cabal-validate:8.6.5 -f .docker/validate-8.6.5.dockerfile .

validate-via-docker-8.8.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.8.4 -f .docker/validate-8.8.4.dockerfile .

validate-via-docker-8.10.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.10.4 -f .docker/validate-8.10.4.dockerfile .

validate-via-docker-old:
	docker build $(DOCKERARGS) -t cabal-validate:older -f .docker/validate-old.dockerfile .

# Weeder
weeder :
	cabal build all --project-file=cabal.project.weeder
	weeder | less

# tags
.PHONY : tags
tags :
	hasktags -b Cabal-syntax/src Cabal/src Cabal-described/src cabal-install/src cabal-testsuite/src

# bootstrapping
##############################################################################

bootstrap-json-%: phony
	cabal build --project=cabal.project.release --with-compiler=ghc-$* --dry-run cabal-install:exe:cabal
	cp dist-newstyle/cache/plan.json bootstrap/linux-$*.plan.json
	@# -v0 to avoid build output on stdout
	cd bootstrap && cabal run -v0 cabal-bootstrap-gen -- linux-$*.plan.json \
		| python3 -m json.tool > linux-$*.json

BOOTSTRAP_GHC_VERSIONS := 8.10.7 9.0.2 9.2.7 9.4.4

bootstrap-jsons: $(BOOTSTRAP_GHC_VERSIONS:%=bootstrap-json-%)

# documentation
##############################################################################

.PHONY: users-guide
users-guide:
	$(MAKE) -C doc users-guide

.PHONY: users-guide-requirements
users-guide-requirements:
	$(MAKE) -C doc users-guide-requirements

ifeq ($(shell uname), Darwin)
PROCS := $(shell sysctl -n hw.logicalcpu)
else
PROCS := $(shell nproc)
endif
