SOFTWARE = HaXml
VERSION  = 1.12

SRCS = \
	src/Text/XML/HaXml.hs src/Text/XML/HaXml/Combinators.hs \
	src/Text/XML/HaXml/Lex.hs \
	src/Text/XML/HaXml/Parse.hs src/Text/XML/HaXml/Pretty.hs \
	src/Text/XML/HaXml/Types.hs src/Text/XML/HaXml/Validate.hs \
	src/Text/XML/HaXml/Wrappers.hs src/Text/XML/HaXml/OneOfN.hs \
	src/Text/XML/HaXml/Verbatim.hs src/Text/XML/HaXml/Escape.hs \
	src/Text/XML/HaXml/Xml2Haskell.hs src/Text/XML/HaXml/Haskell2Xml.hs \
	src/Text/XML/HaXml/Html/Generate.hs src/Text/XML/HaXml/Html/Parse.hs \
	src/Text/XML/HaXml/Html/Pretty.hs \
	src/Text/XML/HaXml/Xtract/Combinators.hs \
	src/Text/XML/HaXml/Xtract/Lex.hs \
	src/Text/XML/HaXml/Xtract/Parse.hs \
	src/Text/XML/HaXml/DtdToHaskell/TypeDef.hs \
	src/Text/XML/HaXml/DtdToHaskell/Convert.hs \
	src/Text/XML/HaXml/DtdToHaskell/Instance.hs \
	src/Text/ParserCombinators/HuttonMeijerWallace.hs \
	src/Text/PrettyPrint/HughesPJ.hs
TOOLSRCS = \
	src/tools/DtdToHaskell.hs src/tools/Xtract.hs src/tools/Validate.hs \
	src/tools/Canonicalise.hs src/tools/MkOneOf.hs

AUX =	configure Makefile src/Makefile src/pkg.conf docs/* examples/* \
	README LICENCE* COPYRIGHT script/echo.c rpm.spec Build.bat
ALLFILES = $(SRCS) $(TOOLSRCS) $(AUX)
# These files in CVS are NOT included in the src distribution.
NOT =	Makefile.inc Makefile.nhc98 src/Makefile.inc src/Makefile.nhc98

.PHONY: all libs tools haddock install register

COMPILERS = $(shell cat obj/compilers)
LIBS  = $(patsubst %, libs-%, $(COMPILERS))
TOOLS = $(patsubst %, tools-%, $(COMPILERS))
INSTALL = $(patsubst %, install-%, $(COMPILERS))
FILESONLY = $(patsubst %, install-filesonly-%, $(COMPILERS))

all: $(LIBS) $(TOOLS)
libs: $(LIBS)
tools: $(TOOLS)
install: $(INSTALL)
install-filesonly: $(FILESONLY)
libs-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) libs
libs-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 libs
libs-hugs:
	@echo "No building required for Hugs version of HaXml libs."
tools-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) toolset
tools-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 toolset
tools-hugs:
	@echo "No building required for Hugs version of HaXml tools."
install-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) install-ghc
install-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 install-nhc98
install-hugs:
	hugs-package src
	cd obj/hugs; $(MAKE) install-tools-hugs
install-filesonly-ghc:
	cd obj/ghc; $(MAKE) HC=$(shell cat obj/ghccmd) install-filesonly-ghc
install-filesonly-nhc98:
	cd obj/nhc98; $(MAKE) HC=nhc98 install-filesonly-nhc98
install-filesonly-hugs: install-hugs
haddock:
	for file in $(SRCS); \
		do cpp -P -traditional -D__NHC__ $$file >$$file.uncpp; \
		done
	-mkdir docs/HaXml
	haddock -h -t HaXml -o docs/HaXml $(patsubst %, %.uncpp, $(SRCS))
	rm $(patsubst %, %.uncpp, $(SRCS))

# packaging a distribution

srcDist: $(ALLFILES) haddock
	rm -f $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION).tar.gz
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	-rm -rf $(SOFTWARE)-$(VERSION)/docs/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/SMIL/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/OpenOffice.org/CVS
	tar cf $(SOFTWARE)-$(VERSION).tar $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)
	gzip $(SOFTWARE)-$(VERSION).tar

zipDist: $(ALLFILES) haddock
	rm -f $(SOFTWARE)-$(VERSION).zip
	mkdir $(SOFTWARE)-$(VERSION)
	tar cf - $(ALLFILES) | ( cd $(SOFTWARE)-$(VERSION); tar xf - )
	-rm -rf $(SOFTWARE)-$(VERSION)/docs/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/SMIL/CVS
	-rm -rf $(SOFTWARE)-$(VERSION)/examples/OpenOffice.org/CVS
	zip -r $(SOFTWARE)-$(VERSION).zip $(SOFTWARE)-$(VERSION)
	rm -rf $(SOFTWARE)-$(VERSION)


# clear up rubbish
clean:
	-rm -r obj/ghc obj/nhc98 obj/hugs
	-cd examples;    rm -f *.hi *.o
realclean: clean
	-rm -f DtdToHaskell Xtract Validate Canonicalise MkOneOf

# DO NOT DELETE: Beginning of Haskell dependencies
src/Text/XML/HaXml/Html/Parse.o : src/Text/XML/HaXml/Html/Parse.hs
src/Text/XML/HaXml/Html/Pretty.o : src/Text/XML/HaXml/Html/Pretty.hs
src/Text/XML/HaXml/Html/Generate.o : src/Text/XML/HaXml/Html/Generate.hs
src/Text/XML/HaXml/Parse.o : src/Text/XML/HaXml/Parse.hs
src/Text/XML/HaXml/Combinators.o : src/Text/XML/HaXml/Combinators.hs
src/Text/XML/HaXml/Pretty.o : src/Text/XML/HaXml/Pretty.hs
src/Text/XML/HaXml/Lex.o : src/Text/XML/HaXml/Lex.hs
src/Text/XML/HaXml/Xtract/Parse.o : src/Text/XML/HaXml/Xtract/Parse.hs
src/Text/XML/HaXml/Xtract/Parse.o : src/Text/XML/HaXml/Combinators.hi
src/Text/XML/HaXml/Xtract/Combinators.o : src/Text/XML/HaXml/Xtract/Combinators.hs
src/Text/XML/HaXml/Xtract/Combinators.o : src/Text/XML/HaXml/Combinators.hi
src/Text/XML/HaXml/Xtract/Lex.o : src/Text/XML/HaXml/Xtract/Lex.hs
src/Text/XML/HaXml/DtdToHaskell/TypeDef.o : src/Text/XML/HaXml/DtdToHaskell/TypeDef.hs
src/Text/XML/HaXml/DtdToHaskell/Convert.o : src/Text/XML/HaXml/DtdToHaskell/Convert.hs
src/Text/XML/HaXml/DtdToHaskell/Convert.o : src/Text/XML/HaXml/DtdToHaskell/TypeDef.hi
src/Text/XML/HaXml/DtdToHaskell/Instance.o : src/Text/XML/HaXml/DtdToHaskell/Instance.hs
src/Text/XML/HaXml/DtdToHaskell/Instance.o : src/Text/XML/HaXml/DtdToHaskell/TypeDef.hi
src/Text/XML/HaXml/OneOfN.o : src/Text/XML/HaXml/OneOfN.hs
src/Text/XML/HaXml/Wrappers.o : src/Text/XML/HaXml/Wrappers.hs
src/Text/XML/HaXml/Wrappers.o : src/Text/XML/HaXml/Pretty.hi
src/Text/XML/HaXml/Wrappers.o : src/Text/XML/HaXml/Html/Parse.hi
src/Text/XML/HaXml/Wrappers.o : src/Text/XML/HaXml/Parse.hi
src/Text/XML/HaXml/Wrappers.o : src/Text/XML/HaXml/Combinators.hi
src/Text/XML/HaXml/Xml2Haskell.o : src/Text/XML/HaXml/Xml2Haskell.hs
src/Text/XML/HaXml/Xml2Haskell.o : src/Text/XML/HaXml/Parse.hi
src/Text/XML/HaXml/Xml2Haskell.o : src/Text/XML/HaXml/Pretty.hi
src/Text/XML/HaXml/Validate.o : src/Text/XML/HaXml/Validate.hs
src/Text/XML/HaXml/Validate.o : src/Text/XML/HaXml/Xml2Haskell.hi
src/Text/XML/HaXml/Validate.o : src/Text/XML/HaXml/Combinators.hi
src/Text/XML/HaXml/Haskell2Xml.o : src/Text/XML/HaXml/Haskell2Xml.hs
src/Text/XML/HaXml/Haskell2Xml.o : src/Text/XML/HaXml/Pretty.hi
src/Text/XML/HaXml/Haskell2Xml.o : src/Text/XML/HaXml/Parse.hi
src/Text/XML/HaXml/Verbatim.o : src/Text/XML/HaXml/Verbatim.hs
src/Text/XML/HaXml/Escape.o : src/Text/XML/HaXml/Escape.hs
src/Text/XML/HaXml/Types.o : src/Text/XML/HaXml/Types.hs
src/Text/XML/HaXml.o : src/Text/XML/HaXml.hs
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Escape.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Verbatim.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Wrappers.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Validate.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Html/Parse.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Html/Generate.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Pretty.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Parse.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Combinators.hi
src/Text/XML/HaXml.o : src/Text/XML/HaXml/Types.hi
src/Text/ParserCombinators/HuttonMeijerWallace.o : src/Text/ParserCombinators/HuttonMeijerWallace.hs
src/Text/PrettyPrint/HughesPJ.o : src/Text/PrettyPrint/HughesPJ.hs
src/tools/Canonicalise.o : src/tools/Canonicalise.hs
src/tools/Canonicalise.o : src/Text/PrettyPrint/HughesPJ.hi
src/tools/Canonicalise.o : src/Text/XML/HaXml/Wrappers.hi
src/tools/Canonicalise.o : src/Text/XML/HaXml/Pretty.hi
src/tools/Canonicalise.o : src/Text/XML/HaXml/Html/Parse.hi
src/tools/Canonicalise.o : src/Text/XML/HaXml/Parse.hi
src/tools/Xtract.o : src/tools/Xtract.hs
src/tools/Xtract.o : src/Text/XML/HaXml/Html/Generate.hi
src/tools/Xtract.o : src/Text/XML/HaXml/Pretty.hi
src/tools/Xtract.o : src/Text/PrettyPrint/HughesPJ.hi
src/tools/Xtract.o : src/Text/XML/HaXml/Xtract/Parse.hi
src/tools/Xtract.o : src/Text/XML/HaXml/Html/Parse.hi
src/tools/Xtract.o : src/Text/XML/HaXml/Parse.hi
src/tools/Xtract.o : src/Text/XML/HaXml/Types.hi
src/tools/Validate.o : src/tools/Validate.hs
src/tools/Validate.o : src/Text/XML/HaXml/Wrappers.hi
src/tools/Validate.o : src/Text/XML/HaXml/Validate.hi
src/tools/Validate.o : src/Text/XML/HaXml/Parse.hi
src/tools/Validate.o : src/Text/XML/HaXml/Types.hi
src/tools/MkOneOf.o : src/tools/MkOneOf.hs
src/tools/DtdToHaskell.o : src/tools/DtdToHaskell.hs
src/tools/DtdToHaskell.o : src/Text/PrettyPrint/HughesPJ.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/DtdToHaskell/Instance.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/DtdToHaskell/Convert.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/DtdToHaskell/TypeDef.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/Parse.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/Types.hi
src/tools/DtdToHaskell.o : src/Text/XML/HaXml/Wrappers.hi
# DO NOT DELETE: End of Haskell dependencies
