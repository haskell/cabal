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

Setup-nhc:
	hmake -nhc98 -package base -prelude Setup

config: setup
	./setup configure --ghc --prefix=$(PREF)

build: build-stamp
build-stamp: config
	./setup build

install: build-stamp
	./setup install $(USER_FLAG)

# Lame for now. I don't mind though, because cabal itself should be
# able to do this soon.  Needs cpphs.
haddock:
	rm -rf dist/doc
	mkdir -p dist/doc/Distribution/Simple
	mkdir -p dist/tmp/doc/html
	cpphs --noline Distribution/Package.hs > dist/doc/Distribution/Package.hs.raw
	cpphs --noline Distribution/Misc.hs > dist/doc/Distribution/Misc.hs.raw
	cpphs --noline Distribution/Version.hs > dist/doc/Distribution/Version.hs.raw
	cpphs --noline Distribution/Setup.hs > dist/doc/Distribution/Setup.hs.raw
	cpphs --noline Distribution/ModuleTest.hs > dist/doc/Distribution/ModuleTest.hs.raw
	cpphs --noline Distribution/Simple.hs > dist/doc/Distribution/Simple.hs.raw
	cpphs --noline Distribution/Make.hs > dist/doc/Distribution/Make.hs.raw
	cpphs --noline Distribution/InstalledPackageInfo.hs > dist/doc/Distribution/InstalledPackageInfo.hs.raw
	cpphs --noline Distribution/Simple/Build.hs > dist/doc/Distribution/Simple/Build.hs.raw
	cpphs --noline Distribution/Simple/Install.hs > dist/doc/Distribution/Simple/Install.hs.raw
	cpphs --noline Distribution/Simple/Configure.hs > dist/doc/Distribution/Simple/Configure.hs.raw
	cpphs --noline Distribution/Simple/Register.hs > dist/doc/Distribution/Simple/Register.hs.raw
	cpphs --noline Distribution/Simple/Utils.hs > dist/doc/Distribution/Simple/Utils.hs.raw
	cpphs --noline Distribution/Simple/SrcDist.hs > dist/doc/Distribution/Simple/SrcDist.hs.raw
	cpphs --noline Distribution/Simple/GHCPackageConfig.hs > dist/doc/Distribution/Simple/GHCPackageConfig.hs.raw
	cpphs --noline Distribution/GetOpt.hs > dist/doc/Distribution/GetOpt.hs.raw
	find dist/doc/Distribution -name "*.raw"|xargs haddock --html --odir=dist/tmp/doc/html --title="The Haskell Cabal" --source=http://cvs.haskell.org/darcs/cabal/ 
	rm -r dist/doc/*
	mv dist/tmp/doc/html dist/doc
	rmdir dist/tmp/doc
	rmdir dist/tmp

clean: clean-cabal clean-hunit clean-test

clean-cabal:
	-rm -f Distribution/*.o Distribution/*.hi
	-rm -f Distribution/Simple/*.o Distribution/Simple/*.hi
	-rm -f Compat/*.o Compat/*.hi
	-rm -f library-infrastructure--darcs.tar.gz
	-rm -rf setup *.o *.hi moduleTest dist installed-pkg-config
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
	mkdir -p dist/debug
	ghc $(GHCFLAGS) -DDEBUG -odir dist/debug -hidir dist/debug -idist/debug/:.:test/HUnit-1.0/src Distribution/ModuleTest -o moduleTest

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

dist: pushall haddock
	darcs dist
	mv cabal.tar.gz /tmp
	cd /tmp && tar -zxvf cabal.tar.gz
	mkdir -p /tmp/cabal/doc/html
	cp -r dist/doc/html /tmp/cabal/doc/html
	cd /tmp && tar -zcvf cabal-code.tgz cabal
	scp /tmp/cabal-code.tgz ijones@www.haskell.org:~/cabal/cabal-code.tgz
	rm -f /tmp/cabal-code.tgz
	rm -f /tmp/cabal.tar.gz
	rm -rf /tmp/cabal
