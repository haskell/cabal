
VERSION=1.18.1

#KIND=devel
KIND=rc
#KIND=cabal-latest

PREFIX=/usr/local
HC=ghc
GHCFLAGS=-Wall -threaded
SSH_USER=$USER

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

setup: $(SOURCES) Setup.hs
	-mkdir -p dist/setup
	$(HC) $(GHCFLAGS) --make -i. -odir dist/setup -hidir dist/setup Setup.hs -o setup

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

PANDOC=pandoc
PANDOC_OPTIONS= \
	--standalone \
	--smart \
	--css=$(PANDOC_HTML_CSS)
PANDOC_HTML_OUTDIR=dist/doc/users-guide
PANDOC_HTML_CSS=Cabal.css

users-guide: $(USERGUIDE_STAMP) doc/*.markdown
$(USERGUIDE_STAMP): doc/*.markdown
	mkdir -p $(PANDOC_HTML_OUTDIR)
	for file in $^; do \
		[ $${file} != doc/index.markdown ] && TOC=--table-of-contents || TOC=; \
		$(PANDOC) $(PANDOC_OPTIONS) $${TOC} --from=markdown --to=html --output $(PANDOC_HTML_OUTDIR)/$$(basename $${file} .markdown).html $${file}; \
	done
	cp doc/$(PANDOC_HTML_CSS) $(PANDOC_HTML_OUTDIR)

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
	tar -C $(DISTLOC) -czf $(DISTLOC)/Cabal-$(VERSION).tar.gz Cabal-$(VERSION)
	mv $(DISTLOC)/Cabal-$(VERSION)/doc $(DISTLOC)/
	mv $(DISTLOC)/Cabal-$(VERSION)/changelog $(DISTLOC)/
	rm -r $(DISTLOC)/Cabal-$(VERSION)/
	@echo "Cabal tarball built: $(DIST_STAMP)"
	@echo "Release fileset prepared: $(DISTLOC)/"

release: $(DIST_STAMP)
	scp -r $(DISTLOC) $(SSH_USER)@haskell.org:/srv/web/haskell.org/cabal/release/cabal-$(VERSION)
	ssh $(SSH_USER)@haskell.org 'cd /srv/web/haskell.org/cabal/release && rm -f $(KIND) && ln -s cabal-$(VERSION) $(KIND)'

# tags...

TAGSSRCDIRS = Distribution Language
tags TAGS: $(SOURCES)
	find $(TAGSSRCDIRS) -name \*.\*hs | xargs hasktags
