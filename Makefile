GHCFLAGS= -itest/HUnit-1.0/src --make -Wall
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
	ghc $(GHCFLAGS) Setup -o setup

build: hunitInstall config
	./setup build

config: setup
	./setup configure --prefix=$(PREF)

install: build
	./setup install $(USER_FLAG)

clean:
	-rm -f Distribution/*.{o,hi} Distribution/Simple/*.{o,hi} 
	-rm -f library-infrastructure--darcs.tar.gz
	-rm -rf setup *.{o,hi} moduleTest dist installed-pkg-config

remove:
	-ghc-pkg $(GHCPKGFLAGS) -r Cabal-0.1
	-ghc-pkg $(GHCPKGFLAGS) -r HUnit-1.0
	-rm -r $(PREF)/lib/{Cabal-0.1,HUnit-1.0}

# dependencies (included):

hunit: hunit-stamp
hunit-stamp:
	cd test/HUnit-1.0 && make && ./setup configure --prefix=$(PREF) && ./setup build
	touch $@

hunitInstall: hunit-stamp hunitInstall-stamp
hunitInstall-stamp:
	cd test/HUnit-1.0 && ./setup install $(USER_FLAG)
	touch $@

# testing...

moduleTest:
	ghc $(GHCFLAGS) Distribution/ModuleTest -o moduleTest

tests: moduleTest
	cd test/A && make

check: tests main
	./moduleTest

# distribution...

pushall:
	darcs push --all ijones@monk.syntaxpolice.org:/home/ijones/public_html/darcs_repos/library-infrastructure
	darcs push --all ijones@cvs.haskell.org:/home/ijones/library-infrastructure

dist: pushall
	darcs dist
	scp library-infrastructure--darcs.tar.gz ijones@www.haskell.org:~/libraryInfrastructure/libraryInfrastructure-code.tgz
	rm -f library-infrastructure--darcs.tar.gz
