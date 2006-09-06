TOP=..

ifeq "$(findstring boilerplate.mk, $(wildcard $(TOP)/mk/*))" ""
# ----------------------------------------------------------------------------
# Standalone Makefile:

CABALVERSION=1.1.5.9.2
KIND=rc
#KIND=latest
GHCFLAGS= --make -fno-warn-unused-matches -cpp
# later: -Wall
PREF=/usr/local
USER_FLAG =
GHCPKGFLAGS =
HC=ghc
HC_PKG=ghc-pkg
# Comment out this line if your system doesn't have System.Posix.
ISPOSIX=-DHAVE_UNIX_PACKAGE

ifdef user
USER_FLAG = --user
GHCPKGFLAGS = -f ~/.ghc-packages
GHCFLAGS += -package-conf ~/.ghc-packages
endif

# the cabal tarball...
CABALBALL=cabal-$(CABALVERSION).tar.gz

all: moduleTest

# build the library itself

setup::
	mkdir -p dist/tmp
	$(HC) $(GHCFLAGS) -i. -odir dist/tmp -hidir dist/tmp Setup.lhs -o setup

Setup-nhc:
	hmake -nhc98 -package base -prelude Setup

config: setup
	./setup configure --ghc --prefix=$(PREF)

build: build-stamp
build-stamp: config
	./setup build
#	cd cabal-install     && mkdir -p dist/tmp && $(HC) $(GHCFLAGS) -i.. -odir dist/tmp -hidir dist/tmp Setup.lhs -o setup && ./setup configure --ghc --prefix=$(PREF) && ./setup build 
#	cd cabal-setup     && mkdir -p dist/tmp && $(HC) $(GHCFLAGS) -i.. -odir dist/tmp -hidir dist/tmp Setup.hs -o setup && ./setup configure --ghc --prefix=$(PREF) && ./setup build 
install: build-stamp
	./setup install $(USER_FLAG)
#	cd cabal-install     && ./setup install
#	cd cabal-setup       && ./setup install

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

haddock: setup
	./setup configure
	./setup haddock

clean-doc:
	cd doc && $(MAKE) clean

doc: haddock
	docbook2html doc/Cabal.xml --output doc/users-guide

clean: clean-cabal clean-hunit clean-test clean-doc

clean-cabal:
	-rm -f Distribution/*.o Distribution/*.hi
	-rm -f Distribution/Simple/*.o Distribution/Simple/*.hi
	-rm -f Compat/*.o Compat/*.hi
	-rm -f darcs* out.build *~ semantic.cache* x*.html
	-rm -f library-infrastructure--darcs.tar.gz
	-rm -rf setup *.o *.hi moduleTest dist installed-pkg-config
	-rm -f build-stamp
	-rm -rf dist/hugs

clean-hunit:
	-rm -f hunit-stamp hunitInstall-stamp
	cd tests/HUnit-1.0 && $(MAKE) clean

clean-test:
	cd tests/A && $(MAKE) clean
	cd tests/wash2hs && $(MAKE) clean

remove: remove-cabal remove-hunit
remove-cabal:
	-$(HC_PKG) $(GHCPKGFLAGS) -r Cabal
	-rm -rf $(PREF)/lib/Cabal-0.1
remove-hunit:
	-$(HC_PKG) $(GHCPKGFLAGS) -r HUnit
	-rm -rf $(PREF)/lib/HUnit-1.0

# dependencies (included):

hunit: hunit-stamp
hunit-stamp:
	cd tests/HUnit-1.0 && $(MAKE) && ./setup configure --prefix=$(PREF) && ./setup build
	touch $@

hunitInstall: hunitInstall-stamp
hunitInstall-stamp: hunit-stamp
	cd tests/HUnit-1.0 && ./setup install $(USER_FLAG)
	touch $@

# testing...

moduleTest:
	mkdir -p dist/debug
	$(HC) $(GHCFLAGS) $(ISPOSIX) -DDEBUG -odir dist/debug -hidir dist/debug -idist/debug/:src:tests/HUnit-1.0/src tests/ModuleTest.hs -o moduleTest 

tests: moduleTest clean
	cd tests/A && $(MAKE) clean
	cd tests/HUnit-1.0 && $(MAKE) clean
	cd tests/A && $(MAKE)
	cd tests/HUnit-1.0 && $(MAKE)

check:
	rm -f moduleTest
	$(MAKE) moduleTest
	./moduleTest

# distribution...

pushall:
	darcs push ijones@darcs.haskell.org:/home/darcs/cabal
	darcs push ijones@darcs.haskell.org:/home/darcs/packages/Cabal

pullall:
	darcs pull ijones@darcs.haskell.org:/home/darcs/cabal
	darcs pull ijones@darcs.haskell.org:/home/darcs/packages/Cabal


pushdist: pushall dist
	scp $(TMPDISTLOC)/cabal.tar.gz ijones@www.haskell.org:~/cabal/cabal-code.tgz
#	PUSH ELSEWHERE: scp changelog ijones@www.haskell.org:~/cabal/release/changelog
#	PUSH ELSEWHERE: scp releaseNotes ijones@www.haskell.org:~/cabal/release/notes
#	rm -f /tmp/cabal-code.tgz

deb: dist
	cd $(TMPDISTLOC) && ln -s $(CABALBALL) haskell-cabal_$(CABALVERSION).orig.tar.gz
	cd $(TMPDISTLOC) && tar -zxvf $(CABALBALL)
	mv $(TMPDISTLOC)/cabal $(TMPDISTLOC)/haskell-cabal-$(CABALVERSION)
	cd $(TMPDISTLOC)/haskell-cabal-$(CABALVERSION) && debuild

$(CABALBALL):
	darcs record
	rm -rf /tmp/cabal* /tmp/Cabal*
	rm -rf $(TMPDISTLOC)
	darcs dist --dist-name=cabal-$(CABALVERSION)

TMPDISTLOC=/tmp/cabaldist

# after this command, there will be cabal.tar.gz in $(TMPDISTLOC),
# which will have built docs, haddock, and source code.

dist: haddock $(CABALBALL)
	rm -rf $(TMPDISTLOC)
	mkdir $(TMPDISTLOC)
	mv $(CABALBALL) $(TMPDISTLOC)
	cd $(TMPDISTLOC) && tar -zxvf $(CABALBALL)
	#mkdir $(TMPDISTLOC)/cabal/doc
	$(MAKE) doc
	cp -r dist/doc/html $(TMPDISTLOC)/cabal-$(CABALVERSION)/doc/API
	cp -r doc/users-guide $(TMPDISTLOC)/cabal-$(CABALVERSION)/doc/users-guide
	cd ~/prgs/build/haskell-report/packages && docbook2html -o /tmp/pkg-spec-html pkg-spec.sgml && docbook2pdf pkg-spec.sgml -o /tmp
	cp -r /tmp/pkg-spec{-html,.pdf} $(TMPDISTLOC)/cabal-$(CABALVERSION)/doc

	cd $(TMPDISTLOC) && rm -f $(CABALBALL) && tar -zcvf $(CABALBALL) cabal-$(CABALVERSION)
	@echo "Cabal tarball built: $(TMPDISTLOC)/$(CABALBALL)"

release: dist
	mkdir $(TMPDISTLOC)/release
	cp $(TMPDISTLOC)/cabal-$(CABALVERSION)/releaseNotes $(TMPDISTLOC)/release
	cp $(TMPDISTLOC)/cabal-$(CABALVERSION)/changelog $(TMPDISTLOC)/release
	cp -r $(TMPDISTLOC)/cabal-$(CABALVERSION)/doc $(TMPDISTLOC)/release
	cp $(TMPDISTLOC)/cabal-$(CABALVERSION).tar.gz  $(TMPDISTLOC)/release/cabal-$(CABALVERSION).tar.gz
	scp -r $(TMPDISTLOC)/release www.haskell.org:/home/haskell/cabal/release/cabal-$(CABALVERSION)
	ssh www.haskell.org 'cd /home/haskell/cabal/release && rm -f $(KIND) && ln -s cabal-$(CABALVERSION) $(KIND)'

else # boilerplate.mk exists
# ----------------------------------------------------------------------------
# GHC build tree Makefile:

include $(TOP)/mk/boilerplate.mk

SUBDIRS = doc #cabal-setup

ALL_DIRS = \
	Distribution \
	Distribution/Simple \
	Distribution/PreProcess \
	Distribution/Compat \
	Language/Haskell

EXCLUDED_SRCS = DefaultSetup.lhs

PACKAGE		= Cabal
VERSION		= 1.1.4

PACKAGE_DEPS	= base

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (Cabal package)"

SRC_HC_OPTS   += -cpp

include $(TOP)/mk/target.mk

endif
