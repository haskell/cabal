GHCFLAGS= --make -Wall
PREF=/usr/local
USER_FLAG =
GHCPKGFLAGS = 

ifdef user
USER_FLAG = --user
GHCPKGFLAGS = -f ~/.ghc-packages
GHCFLAGS += -package-conf ~/.ghc-packages
endif

all: moduleTest

# build the library itself

setup:
	mkdir -p dist/tmp
	ghc $(GHCFLAGS) -odir dist/tmp -hidir dist/tmp Setup -o setup

config: setup
	./setup configure --prefix=$(PREF)

build: build-stamp
build-stamp: config
	./setup build

install: build-stamp
	./setup install $(USER_FLAG)

# FIX: doesn't work because of preprocsesing.
doc:
	haddock --html --odir=dist/tmp/doc --title="The Haskell Cabal" --source=http://cvs.haskell.org/darcs/cabal/ `find Distribution -name "*.hs"`

clean: clean-cabal clean-hunit clean-test

clean-cabal:
	-rm -f Distribution/*.{o,hi} Distribution/Simple/*.{o,hi} 
	-rm -f Compat/*.{o,hi}
	-rm -f library-infrastructure--darcs.tar.gz
	-rm -rf setup *.{o,hi} moduleTest dist installed-pkg-config
	-rm -f build-stamp

clean-hunit:
	-rm -f hunit-stamp hunitInstall-stamp
	cd test/HUnit-1.0 && make clean

clean-test:
	cd test/A && make clean
	cd test/wash2hs && make clean

remove: remove-cabal remove-hunit
remove-cabal:
	-ghc-pkg $(GHCPKGFLAGS) -r Cabal
	-rm -rf $(PREF)/lib/Cabal-0.1
remove-hunit:
	-ghc-pkg $(GHCPKGFLAGS) -r HUnit
	-rm -rf $(PREF)/lib/HUnit-1.0

# dependencies (included):

hunit: hunit-stamp
hunit-stamp:
	cd test/HUnit-1.0 && make && ./setup configure --prefix=$(PREF) && ./setup build
	touch $@

hunitInstall: hunitInstall-stamp
hunitInstall-stamp: hunit-stamp
	cd test/HUnit-1.0 && ./setup install $(USER_FLAG)
	touch $@

# testing...

moduleTest:
	-rm -rf dist/debug
	mkdir -p dist/debug
	ghc $(GHCFLAGS) -DDEBUG -odir dist/debug -hidir dist/debug -i.:test/HUnit-1.0/src Distribution/ModuleTest -o moduleTest

tests: moduleTest clean
	cd test/A && make clean
	cd test/HUnit-1.0 && make clean
	cd test/A && make
	cd test/HUnit-1.0 && make

check: tests
	./moduleTest

# distribution...

pushall:
	darcs push --all ijones@cvs.haskell.org:/home/darcs/cabal

dist: pushall
	darcs dist
	scp cabal.tar.gz ijones@www.haskell.org:~/libraryInfrastructure/libraryInfrastructure-code.tgz
	rm -f cabal.tar.gz
