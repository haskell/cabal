
HC=ghc
GHCFLAGS=--make -package Cabal -Wall -fno-warn-unused-matches -cpp
PREF=/usr/local

all: setup build

help:
	@echo
	@echo "For a user-install (~/bin/cabal-instal) do:"
	@echo "  $$ make install-user"
	@echo
	@echo "For a sistem-wide install do:"
	@echo "  $$ make"
	@echo "  $$ sudo make install"
	@echo

setup:
	mkdir -p dist/tmp
	$(HC) $(GHCFLAGS) -odir dist/tmp -hidir dist/tmp Setup.lhs -o setup

config: setup
	./setup configure --ghc --prefix=$(PREF)

build: config
	./setup build

.PHONY: install
install:
	./setup install
	mkdir -p /etc/cabal-install
	cp etc-cabal-install/serv.list /etc/cabal-install/

config-user: setup
	./setup configure --ghc --prefix=${HOME}

build-user: config-user
	./setup build

.PHONY: install-user
install-user: build-user
	./setup install
	mkdir -p ${HOME}/.cabal-install
	cp etc-cabal-install/serv.list ${HOME}/.cabal-install/

clean:
	rm -rf *.o *.hi setup cabal-install dist/

