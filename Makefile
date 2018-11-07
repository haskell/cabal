.PHONY : all lexer sdpx lib exe doctest
.PHONY : gen-extra-source-files gen-extra-source-files-lib gen-extra-source-files-cli
.PHONY : cabal-install-dev cabal-install-prod

LEXER_HS:=Cabal/Distribution/Parsec/Lexer.hs
SPDX_LICENSE_HS:=Cabal/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal/Distribution/SPDX/LicenseExceptionId.hs

all : exe lib

lexer : $(LEXER_HS)

spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

$(LEXER_HS) : boot/Lexer.x
	alex --latin1 --ghc -o $@ $^
	cat -s $@ > Lexer.tmp
	mv Lexer.tmp $@

$(SPDX_LICENSE_HS) : boot/SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx -- boot/SPDX.LicenseId.template.hs license-list-data/licenses-3.0.json license-list-data/licenses-3.2.json $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : boot/SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenUtils.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx-exc -- boot/SPDX.LicenseExceptionId.template.hs license-list-data/exceptions-3.0.json license-list-data/exceptions-3.2.json $(SPDX_EXCEPTION_HS)

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

lib : $(LEXER_HS)
	cabal new-build --enable-tests Cabal

exe : $(LEXER_HS)
	cabal new-build --enable-tests cabal-install

doctest :
	doctest --fast Cabal/Distribution Cabal/Language

gen-extra-source-files : gen-extra-source-files-lib gen-extra-source-files-cli

gen-extra-source-files-lib :
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- $$(pwd)/Cabal/Cabal.cabal

# We need to generate cabal-install-dev so the test modules are in .cabal file!
gen-extra-source-files-cli :
	$(MAKE) cabal-install-dev
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- $$(pwd)/cabal-install/cabal-install.cabal.pp $$(pwd)/cabal-install/cabal-install.cabal
	$(MAKE) cabal-install-prod

cabal-install-test:
	cabal new-build -j3 all --disable-tests --disable-benchmarks
	rm -rf .ghc.environment.*
	cd cabal-testsuite && `cabal-plan list-bin cabal-tests` --with-cabal=`cabal-plan list-bin cabal` --hide-successes -j3 ${TEST}
