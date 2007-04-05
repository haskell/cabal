HADDOCK = haddock

TODAY = $(shell date +%Y%m%d)
DIST_NAME = http-$(TODAY)

HADDOCK_FILES = Network/HTTP.hs Network/Browser.hs 

.PHONY: all configure build install dist haddock clean

default all: configure build

configure:
	./Setup.lhs configure

build:
	./Setup.lhs build

install:
	./Setup.lhs install

dist:
	darcs dist --dist-name=$(DIST_NAME)

haddock: $(HADDOCK_FILES)
	mkdir -p haddock
	$(HADDOCK) -o haddock -h $^

clean:
	-./Setup.lhs clean
	-rm -rf haddock
	-rm -rf dist
	$(MAKE) -C test clean

setup: Setup.lhs
	ghc --make -package Cabal -o setup Setup.lhs
