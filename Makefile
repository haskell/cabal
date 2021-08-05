.PHONY : all lexer sdpx lib exe doctest
.PHONY : cabal-install-dev cabal-install-prod
.PHONY : phony

CABALBUILD := cabal v2-build
CABALRUN   := cabal v2-run

# default rules

all : exe lib

lib : $(LEXER_HS)
	$(CABALBUILD) Cabal:libs

exe : $(LEXER_HS)
	$(CABALBUILD) cabal-install:exes

# Build library with oldest supported GHC
lib-ghc-7.6 :
	$(CABALBUILD) --project-file=cabal.project.libonly --with-compiler=ghc-7.6.3 Cabal:libs

lib-ghc-7.8 :
	$(CABALBUILD) --project-file=cabal.project.libonly --with-compiler=ghc-7.8.4 Cabal:libs

# source generation: Lexer

LEXER_HS:=Cabal/src/Distribution/Fields/Lexer.hs

lexer : $(LEXER_HS)

$(LEXER_HS) : templates/Lexer.x
	alex --latin1 --ghc -o $@ $^
	cat -s $@ > Lexer.tmp
	mv Lexer.tmp $@

# source generation: SPDX

SPDX_LICENSE_HS:=Cabal/src/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal/src/Distribution/SPDX/LicenseExceptionId.hs

spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

SPDX_LICENSE_VERSIONS:=3.0 3.2 3.6 3.9 3.10

$(SPDX_LICENSE_HS) : templates/SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx -- templates/SPDX.LicenseId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/licenses-%.json) $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : templates/SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx-exc -- templates/SPDX.LicenseExceptionId.template.hs $(SPDX_LICENSE_VERSIONS:%=license-list-data/exceptions-%.json) $(SPDX_EXCEPTION_HS)

# source generation: templates

TEMPLATE_MACROS:=Cabal/src/Distribution/Simple/Build/Macros/Z.hs
TEMPLATE_PATHS:=Cabal/src/Distribution/Simple/Build/PathsModule/Z.hs

templates : phony $(TEMPLATE_MACROS) $(TEMPLATE_PATHS)

$(TEMPLATE_MACROS) : templates/cabal_macros.template.h cabal-dev-scripts/src/GenCabalMacros.hs
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-cabal-macros -- $< $@

$(TEMPLATE_PATHS) : templates/Paths_pkg.template.hs cabal-dev-scripts/src/GenPathsModule.hs
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-paths-module -- $< $@

# generated docs

buildinfo-fields-reference : phony
	cabal build --builddir=dist-newstyle-bi --project-file=cabal.project.buildinfo buildinfo-reference-generator
	$$(cabal-plan list-bin --builddir=dist-newstyle-bi buildinfo-reference-generator) buildinfo-reference-generator/template.zinza | tee $@

# cabal-install.cabal file generation

cabal-install-cabal : phony cabal-install/cabal-install.cabal.dev cabal-install/cabal-install.cabal.prod

cabal-install/cabal-install.cabal.dev : cabal-install/cabal-install.cabal.zinza
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-cabal-install-cabal -- True cabal-install/cabal-install.cabal.zinza cabal-install/cabal-install.cabal.dev

cabal-install/cabal-install.cabal.prod : cabal-install/cabal-install.cabal.zinza
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-cabal-install-cabal -- False cabal-install/cabal-install.cabal.zinza cabal-install/cabal-install.cabal.prod

cabal-install-prod : cabal-install/cabal-install.cabal.prod
	cp cabal-install/cabal-install.cabal.prod cabal-install/cabal-install.cabal

cabal-install-dev : cabal-install/cabal-install.cabal.dev
	cp cabal-install/cabal-install.cabal.dev cabal-install/cabal-install.cabal
	@echo "tell git to ignore changes to cabal-install.cabal:"
	@echo "git update-index --assume-unchanged cabal-install/cabal-install.cabal"

# analyse-imports
analyse-imports : phony
	find Cabal/src cabal-install/src -type f -name '*.hs' | xargs cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta analyse-imports --

# github actions
github-actions : .github/workflows/artifacts.yml
github-actions : .github/workflows/quick-jobs.yml
github-actions : .github/workflows/bootstrap.yml
github-actions : .github/workflows/linux.yml
github-actions : .github/workflows/macos.yml
github-actions : .github/workflows/windows.yml

.github/workflows/%.yml : templates/ci-%.template.yml cabal-dev-scripts/src/GenValidate.hs
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-validate -- $< $@

# ghcid

ghcid-lib :
	ghcid -c 'cabal v2-repl Cabal'

ghcid-cli :
	ghcid -c 'cabal v2-repl cabal-install'

# doctests (relies on .ghc.environment files)

doctest :
	doctest --fast Cabal/src

# This is not run as part of validate.sh (we need hackage-security, which is tricky to get).
doctest-cli :
	doctest -D__DOCTEST__ --fast cabal-install/src cabal-install/cabal-install-solver/src cabal-install/cabal-install-solver/src-assertion

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
	@which cabal-plan
	$(CABALBUILD) -j3 cabal-tests cabal
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal-plan list-bin cabal-tests` --with-cabal=`cabal-plan list-bin cabal` --hide-successes -j3 ${TEST}

# hackage-benchmarks (solver)

hackage-benchmarks-run:
	$(CABALBUILD) -j3 hackage-benchmark cabal
	rm -rf .ghc.environment.*
	$$(cabal-plan list-bin hackage-benchmark) --cabal1=cabal --cabal2=$$(cabal-plan list-bin cabal) --packages="hakyll servant-auth-server" --print-trials --concurrently


# This doesn't run build, as you first need to test with cabal-install-test :)
cabal-install-test-accept:
	@which cabal-plan
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal-plan list-bin cabal-tests` --with-cabal=`cabal-plan list-bin cabal` --hide-successes -j3 --accept ${TEST}

# Docker validation

# Use this carefully, on big machine you can say
#
#   make validate-via-docker-all -j4 -O
#
validate-via-docker-all : validate-via-docker-7.6.3
validate-via-docker-all : validate-via-docker-7.8.4
validate-via-docker-all : validate-via-docker-7.10.3
validate-via-docker-all : validate-via-docker-8.0.2
validate-via-docker-all : validate-via-docker-8.2.2
validate-via-docker-all : validate-via-docker-8.4.4
validate-via-docker-all : validate-via-docker-8.6.5
validate-via-docker-all : validate-via-docker-8.8.3
validate-via-docker-all : validate-via-docker-8.10.1

validate-dockerfiles : .docker/validate-8.10.1.dockerfile
validate-dockerfiles : .docker/validate-8.8.3.dockerfile
validate-dockerfiles : .docker/validate-8.6.5.dockerfile
validate-dockerfiles : .docker/validate-8.4.4.dockerfile
validate-dockerfiles : .docker/validate-8.2.2.dockerfile
validate-dockerfiles : .docker/validate-8.6.5.dockerfile
validate-dockerfiles : .docker/validate-7.10.3.dockerfile
validate-dockerfiles : .docker/validate-7.8.4.dockerfile
validate-dockerfiles : .docker/validate-7.6.3.dockerfile

.docker/validate-%.dockerfile : .docker/validate.dockerfile.zinza cabal-dev-scripts/src/GenValidateDockerfile.hs
	cabal v2-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-validate-dockerfile -- $* $< $@

# This is good idea anyway
# and we have a test relying on this limit being sufficiently small
DOCKERARGS:=--ulimit nofile=1024:1024

validate-via-docker-7.6.3:
	docker build $(DOCKERARGS) -t cabal-validate:7.6.3 -f .docker/validate-7.6.3.dockerfile .

validate-via-docker-7.8.4:
	docker build $(DOCKERARGS) -t cabal-validate:7.8.4 -f .docker/validate-7.8.4.dockerfile .

validate-via-docker-7.10.3:
	docker build $(DOCKERARGS) -t cabal-validate:7.10.3 -f .docker/validate-7.10.3.dockerfile .

validate-via-docker-8.0.2:
	docker build $(DOCKERARGS) -t cabal-validate:8.0.2 -f .docker/validate-8.0.2.dockerfile .

validate-via-docker-8.2.2:
	docker build $(DOCKERARGS) -t cabal-validate:8.2.2 -f .docker/validate-8.2.2.dockerfile .

validate-via-docker-8.4.4:
	docker build $(DOCKERARGS) -t cabal-validate:8.4.4 -f .docker/validate-8.4.4.dockerfile .

validate-via-docker-8.6.5:
	docker build $(DOCKERARGS) -t cabal-validate:8.6.5 -f .docker/validate-8.6.5.dockerfile .

validate-via-docker-8.8.3:
	docker build $(DOCKERARGS) -t cabal-validate:8.8.3 -f .docker/validate-8.8.3.dockerfile .

validate-via-docker-8.10.1:
	docker build $(DOCKERARGS) -t cabal-validate:8.10.1 -f .docker/validate-8.10.1.dockerfile .

validate-via-docker-old:
	docker build $(DOCKERARGS) -t cabal-validate:older -f .docker/validate-old.dockerfile .

# Weeder
weeder :
	cabal build all --project-file=cabal.project.weeder
	weeder | less

# tags
.PHONY : tags
tags :
	hasktags -b Cabal/src Cabal-described/src cabal-install/src cabal-testsuite/src

# boostrapping
##############################################################################

bootstrap-plans-linux: phony
	@if [ $$(uname) != "Linux" ]; then echo "Not Linux"; false; fi
	cabal v2-build --project=cabal.project.release --with-compiler ghc-8.6.5  --dry-run cabal-install:exe:cabal
	cp dist-newstyle/cache/plan.json bootstrap/linux-8.6.5.plan.json
	cabal v2-build --project=cabal.project.release --with-compiler ghc-8.8.3  --dry-run cabal-install:exe:cabal
	cp dist-newstyle/cache/plan.json bootstrap/linux-8.8.3.plan.json
	cabal v2-build --project=cabal.project.release --with-compiler ghc-8.10.1 --dry-run cabal-install:exe:cabal
	cp dist-newstyle/cache/plan.json bootstrap/linux-8.10.1.plan.json

bootstrap-jsons-linux: phony
	@if [ $$(uname) != "Linux" ]; then echo "Not Linux"; false; fi
	cabal v2-build               --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen
	cabal v2-run -vnormal+stderr --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen -- bootstrap/linux-8.6.5.plan.json  | python -m json.tool | tee bootstrap/linux-8.6.5.json
	cabal v2-run -vnormal+stderr --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen -- bootstrap/linux-8.8.3.plan.json  | python -m json.tool | tee bootstrap/linux-8.8.3.json
	cabal v2-run -vnormal+stderr --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen -- bootstrap/linux-8.10.1.plan.json | python -m json.tool | tee bootstrap/linux-8.10.1.json

# documentation
##############################################################################

# TODO: when we have sphinx-build2 ?
SPHINXCMD:=sphinx-build
SPHINX_HTML_OUTDIR:=dist-newstyle/doc/users-guide
USERGUIDE_STAMP:=$(SPHINX_HTML_OUTDIR)/index.html

# do pip install everytime so we have up to date requirements when we build
users-guide: .python-sphinx-virtualenv $(USERGUIDE_STAMP)
$(USERGUIDE_STAMP) : doc/*.rst
	mkdir -p $(SPHINX_HTML_OUTDIR)
	(. ./.python-sphinx-virtualenv/bin/activate && pip install -r doc/requirements.txt && $(SPHINXCMD) doc $(SPHINX_HTML_OUTDIR))

.python-sphinx-virtualenv:
	python3 -m venv .python-sphinx-virtualenv
	(. ./.python-sphinx-virtualenv/bin/activate)
