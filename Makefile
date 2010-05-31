
VERSION=1.9.2

#KIND=devel
KIND=rc
#KIND=latest

PREFIX=/usr/local
HC=ghc
GHCFLAGS=-Wall

all: build

# build the library itself

SOURCES=Distribution/*.hs Distribution/Simple/*.hs Distribution/PackageDescription/*.hs Distribution/Simple/GHC/*.hs Distribution/Simple/Build/*.hs Distribution/Compat/*.hs Distribution/Simple/Program/*.hs
CONFIG_STAMP=dist/setup-config
BUILD_STAMP=dist/build/libHSCabal-$(VERSION).a
HADDOCK_STAMP=dist/doc/html/Cabal/index.html
USERGUIDE_STAMP=dist/doc/users-guide/index.html
SDIST_STAMP=dist/Cabal-$(VERSION).tar.gz
DISTLOC=dist/release
DIST_STAMP=$(DISTLOC)/Cabal-$(VERSION).tar.gz

COMMA=,
VERSION_VALUE=$(subst .,$(COMMA),$(VERSION))

setup: $(SOURCES) Setup.hs
	-mkdir -p dist/setup
	$(HC) $(GHCFLAGS) --make -DCABAL_VERSION=$(VERSION_VALUE) -i. -odir dist/setup -hidir dist/setup Setup.hs -o setup

Setup-nhc:
	hmake -nhc98 -package base -prelude Setup

$(CONFIG_STAMP): setup Cabal.cabal
	./setup configure --with-compiler=$(HC) --prefix=$(PREFIX)

build: $(BUILD_STAMP)
$(BUILD_STAMP): $(CONFIG_STAMP) $(SOURCES)
	./setup build

install: $(BUILD_STAMP)
	./setup install

hugsbootstrap:
	rm -rf dist/tmp dist/hugs
	mkdir -p dist/tmp
	mkdir dist/hugs
	cp -r Distribution dist/tmp
	hugs-package dist/tmp dist/hugs
	cp Setup.lhs Cabal.cabal dist/hugs

hugsinstall: hugsbootstrap
	cd dist/hugs && ./Setup.lhs configure --hugs
	cd dist/hugs && ./Setup.lhs build
	cd dist/hugs && ./Setup.lhs install

# documentation...

haddock: $(HADDOCK_STAMP)
$(HADDOCK_STAMP) : $(CONFIG_STAMP) $(BUILD_STAMP)
	./setup haddock

XMLLINT=xmllint
XMLLINT_OPTIONS=--nonet --noout --valid

XSLTPROC=xsltproc
XSLTPROC_HTML_OUTDIR=dist/doc/users-guide/
XSLTPROC_HTML_DOCTYPE_PUBLIC="-//W3C//DTD HTML 4.01 Transitional//EN"
XSLTPROC_HTML_DOCTYPE_SYSTEM="http://www.w3.org/TR/html4/loose.dtd"
XSLTPROC_HTML_ENCODING=UTF-8
XSLTPROC_HTML_CSS=Cabal.css
XSLTPROC_HTML_PARAMS=\
	--param use.id.as.filename 1 \
	--param toc.section.depth 3 \
	--stringparam base.dir $(XSLTPROC_HTML_OUTDIR) \
	--stringparam chunker.output.doctype-public $(XSLTPROC_HTML_DOCTYPE_PUBLIC) \
	--stringparam chunker.output.doctype-system $(XSLTPROC_HTML_DOCTYPE_SYSTEM) \
	--stringparam chunker.output.encoding $(XSLTPROC_HTML_ENCODING) \
	--stringparam html.stylesheet $(XSLTPROC_HTML_CSS)
XSLTPROC_HTML_STYLESHEET=http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl
XSLTPROC_OPTIONS=--nonet $(XSLTPROC_HTML_PARAMS) $(XSLTPROC_HTML_STYLESHEET)

users-guide: $(USERGUIDE_STAMP)
$(USERGUIDE_STAMP) : doc/Cabal.xml
	$(XMLLINT) $(XMLLINT_OPTIONS) $<
	$(XSLTPROC) $(XSLTPROC_OPTIONS) $<
	cp doc/$(XSLTPROC_HTML_CSS) $(XSLTPROC_HTML_OUTDIR)

docs: haddock users-guide

clean:
	rm -rf dist/
	rm -f setup

# testing...

moduleTest: tests/ModuleTest.hs tests/PackageDescriptionTests.hs
	mkdir -p dist/test
	$(HC) --make -Wall -DDEBUG -odir dist/test -hidir dist/test \
		-itests tests/ModuleTest.hs -o moduleTest

#tests: moduleTest clean
#	cd tests/A && $(MAKE) clean
#	cd tests/HUnit-1.0 && $(MAKE) clean
#	cd tests/A && $(MAKE)
#	cd tests/HUnit-1.0 && $(MAKE)

#check:
#	rm -f moduleTest
#	$(MAKE) moduleTest
#	./moduleTest

# distribution...

$(SDIST_STAMP) : $(BUILD_STAMP)
	./setup sdist

dist: $(DIST_STAMP)
$(DIST_STAMP) : $(HADDOCK_STAMP) $(USERGUIDE_STAMP) $(SDIST_STAMP)
	rm -rf $(DISTLOC)
	mkdir $(DISTLOC)
	tar -xzf $(SDIST_STAMP) -C $(DISTLOC)/
	mkdir $(DISTLOC)/Cabal-$(VERSION)/doc
	cp -r dist/doc/html $(DISTLOC)/Cabal-$(VERSION)/doc/API
	cp -r dist/doc/users-guide $(DISTLOC)/Cabal-$(VERSION)/doc/
	cp changelog $(DISTLOC)/Cabal-$(VERSION)/
	tar -C $(DISTLOC) -c Cabal-$(VERSION) -zf $(DISTLOC)/Cabal-$(VERSION).tar.gz
	mv $(DISTLOC)/Cabal-$(VERSION)/doc $(DISTLOC)/
	mv $(DISTLOC)/Cabal-$(VERSION)/changelog $(DISTLOC)/
	rm -r $(DISTLOC)/Cabal-$(VERSION)/
	@echo "Cabal tarball built: $(DIST_STAMP)"
	@echo "Release fileset prepared: $(DISTLOC)/"

release: $(DIST_STAMP)
	scp -r $(DISTLOC) www.haskell.org:/home/haskell/cabal/release/cabal-$(VERSION)
	ssh www.haskell.org 'cd /home/haskell/cabal/release && rm -f $(KIND) && ln -s cabal-$(VERSION) $(KIND)'

# tags...

TAGSSRCDIRS = Distribution Language
tags TAGS: $(SOURCES)
	find $(TAGSSRCDIRS) -name \*.\*hs | xargs hasktags
