.PHONY : all lexer sdpx lib exe doctest
.PHONY : gen-extra-source-files gen-extra-source-files-lib gen-extra-source-files-cli
.PHONY : cabal-install-dev cabal-install-prod

LEXER_HS:=Cabal/Distribution/Fields/Lexer.hs
SPDX_LICENSE_HS:=Cabal/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal/Distribution/SPDX/LicenseExceptionId.hs

CABALBUILD := cabal new-build --enable-tests
CABALRUN   := cabal new-run --enable-tests

# default rules

all : exe lib

lib : $(LEXER_HS)
	$(CABALBUILD) Cabal:libs

exe : $(LEXER_HS)
	$(CABALBUILD) cabal-install:exes

# source generation: Lexer

lexer : $(LEXER_HS)

$(LEXER_HS) : boot/Lexer.x
	alex --latin1 --ghc -o $@ $^
	cat -s $@ > Lexer.tmp
	mv Lexer.tmp $@

# source generation: SPDX

spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

$(SPDX_LICENSE_HS) : boot/SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx -- boot/SPDX.LicenseId.template.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json license-list-data/licenses-3.5.json $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : boot/SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx-exc -- boot/SPDX.LicenseExceptionId.template.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json license-list-data/exceptions-3.5.json $(SPDX_EXCEPTION_HS)

# cabal-install.cabal file generation

cabal-install-prod : cabal-install/cabal-install.cabal.pp
	runghc cabal-dev-scripts/src/Preprocessor.hs -o cabal-install/cabal-install.cabal cabal-install/cabal-install.cabal.pp
	git update-index --no-assume-unchanged cabal-install/cabal-install.cabal

cabal-install-dev : cabal-install/cabal-install.cabal.pp
	runghc cabal-dev-scripts/src/Preprocessor.hs -o cabal-install/cabal-install.cabal -f CABAL_FLAG_LIB cabal-install/cabal-install.cabal.pp
	@echo "tell git to ignore changes to cabal-install.cabal:"
	@echo "git update-index --assume-unchanged cabal-install/cabal-install.cabal"

cabal-install-monolithic : cabal-install/cabal-install.cabal.pp
	runghc cabal-dev-scripts/src/Preprocessor.hs -o cabal-install/cabal-install.cabal -f CABAL_FLAG_LIB -f CABAL_FLAG_MONOLITHIC cabal-install/cabal-install.cabal.pp
	@echo "tell git to ignore changes to cabal-install.cabal:"
	@echo "git update-index --assume-unchanged cabal-install/cabal-install.cabal"

# extra-source-files generation

gen-extra-source-files : gen-extra-source-files-lib gen-extra-source-files-cli

gen-extra-source-files-lib :
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- $$(pwd)/Cabal/Cabal.cabal

# We need to generate cabal-install-dev so the test modules are in .cabal file!
gen-extra-source-files-cli :
	$(MAKE) cabal-install-dev
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- $$(pwd)/cabal-install/cabal-install.cabal.pp $$(pwd)/cabal-install/cabal-install.cabal
	$(MAKE) cabal-install-prod

# ghcid

ghcid-lib :
	ghcid -c 'cabal new-repl Cabal'

ghcid-cli :
	ghcid -c 'cabal new-repl cabal-install'

# doctests (relies on .ghc.environment files)

doctest :
	doctest --fast Cabal/Distribution Cabal/Language

# tests

check-tests :
	$(CABALRUN) --enable-tests check-tests -- --cwd Cabal ${TEST}

parser-tests :
	$(CABALRUN) parser-tests -- --cwd Cabal ${TEST}

parser-tests-accept :
	$(CABALRUN) parser-tests -- --cwd Cabal --accept ${TEST}

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
