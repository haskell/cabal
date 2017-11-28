.PHONY : all lexer lib exe doctest gen-extra-source-files

LEXER_HS:=Cabal/Distribution/Parsec/Lexer.hs

all : exe lib

lexer : $(LEXER_HS)

$(LEXER_HS) : boot/Lexer.x
	alex --latin1 --ghc -o $@ $^

lib : $(LEXER_HS)
	cabal new-build --enable-tests Cabal

exe : $(LEXER_HS)
	cabal new-build --enable-tests cabal

doctest :
	doctest --fast Cabal/Distribution Cabal/Language

gen-extra-source-files:
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- Cabal/Cabal.cabal
	cabal new-run --builddir=dist-newstyle=meta --project-file=cabal.project.meta gen-extra-source-files -- cabal-install/cabal-install.cabal
