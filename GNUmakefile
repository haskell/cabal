CABALVERSION=0.5
GHCFLAGS= --make -Wall -fno-warn-unused-matches -cpp
# later: -Wall
PREF=/usr/local
USER_FLAG =
GHCPKGFLAGS = 
#HCBASE=/tmp/ghc/bin/
HCBASE=/usr/bin/
HC=$(HCBASE)ghc
HC_PKG=$(HCBASE)/ghc-pkg
# Comment out this line if your system doesn't have System.Posix.
ISPOSIX=-DHAVE_UNIX_PACKAGE

ifdef user
USER_FLAG = --user
GHCPKGFLAGS = -f ~/.ghc-packages
GHCFLAGS += -package-conf ~/.ghc-packages
endif

# the cabal tarball...
CABALBALL=cabal.tar.gz

all: moduleTest

# build the library itself

setup::
	mkdir -p dist/tmp
	$(HC) $(GHCFLAGS) -odir dist/tmp -hidir dist/tmp Setup -o setup

Setup-nhc:
	hmake -nhc98 -package base -prelude Setup

config: setup
	./setup configure --ghc --prefix=$(PREF)

build: build-stamp
build-stamp: config
	./setup build

install: build-stamp
	./setup install $(USER_FLAG)

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
	cd doc && make clean

doc: haddock
	docbook2html doc/Cabal.xml --output doc/users-guide

clean: clean-cabal clean-hunit clean-test

clean-cabal:
	-rm -f Distribution/*.o Distribution/*.hi
	-rm -f Distribution/Simple/*.o Distribution/Simple/*.hi
	-rm -f Compat/*.o Compat/*.hi
	-rm -f library-infrastructure--darcs.tar.gz
	-rm -rf setup *.o *.hi moduleTest dist installed-pkg-config
	-rm -f build-stamp
	-rm -rf dist/hugs

clean-hunit:
	-rm -f hunit-stamp hunitInstall-stamp
	cd tests/HUnit-1.0 && make clean

clean-test:
	cd tests/A && make clean
	cd tests/wash2hs && make clean

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
	cd tests/HUnit-1.0 && make && ./setup configure --prefix=$(PREF) && ./setup build
	touch $@

hunitInstall: hunitInstall-stamp
hunitInstall-stamp: hunit-stamp
	cd tests/HUnit-1.0 && ./setup install $(USER_FLAG)
	touch $@

# testing...

moduleTest:
	mkdir -p dist/debug
	$(HC) $(GHCFLAGS) $(ISPOSIX) -DDEBUG -odir dist/debug -hidir dist/debug -idist/debug/:.:tests/HUnit-1.0/src tests/ModuleTest.hs -o moduleTest 

tests: moduleTest clean
	cd tests/A && make clean
	cd tests/HUnit-1.0 && make clean
	cd tests/A && make
	cd tests/HUnit-1.0 && make

check:
	rm -f moduleTest
	make moduleTest
	./moduleTest

# distribution...

pushall:
	darcs push --all ijones@cvs.haskell.org:/home/darcs/cabal

pushdist: pushall dist
	scp /tmp/cabal-code.tgz ijones@www.haskell.org:~/cabal/cabal-code.tgz
#	rm -f /tmp/cabal-code.tgz

deb: dist
	cd $(TMPDISTLOC) && ln -s $(CABALBALL) haskell-cabal_$(CABALVERSION).orig.tar.gz
	cd $(TMPDISTLOC) && tar -zxvf $(CABALBALL)
	mv $(TMPDISTLOC)/cabal $(TMPDISTLOC)/haskell-cabal-$(CABALVERSION)
	cd $(TMPDISTLOC)/haskell-cabal-$(CABALVERSION) && debuild

$(CABALBALL):
	rm -rf /tmp/cabal* /tmp/Cabal*
	rm -rf $(TMPDISTLOC)
	darcs dist
	mv Cabal.tar.gz $(CABALBALL)

TMPDISTLOC=/tmp/cabaldist

dist: haddock $(CABALBALL)
	rm -rf $(TMPDISTLOC)
	mkdir $(TMPDISTLOC)
	mv $(CABALBALL) $(TMPDISTLOC)
	cd $(TMPDISTLOC) && tar -zxvf $(CABALBALL) && mv Cabal cabal
	#mkdir $(TMPDISTLOC)/cabal/doc
	make doc
	cp -r dist/doc/html $(TMPDISTLOC)/cabal/doc/API
	cp -r doc/users-guide $(TMPDISTLOC)/cabal/doc/users-guide
	cd ~/usr/doc/haskell/haskell-report/packages && docbook2html -o /tmp/pkg-spec-html pkg-spec.sgml && docbook2pdf pkg-spec.sgml -o /tmp
	cp -r /tmp/pkg-spec{-html,.pdf} $(TMPDISTLOC)/cabal/doc

	cd $(TMPDISTLOC) && tar -zcvf $(CABALBALL) cabal
#	rm -f /tmp/Cabal.tar.gz
#	rm -rf /tmp/cabal
