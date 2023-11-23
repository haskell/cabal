CABAL="../new_new_dir/build/x86_64-linux/ghc-9.6.2/cabal-install-3.11.0.0/x/cabal/build/cabal/cabal"

clean:
	rm -rf repo/

init: clean
	mkdir -p repo/
	$(eval PDIR := $(shell pwd))
	$(eval TMP := $(shell mktemp -d))
	cd $(TMP); cp -r $(PDIR)/lib-0.1 "lib01-0.1.0.0"
	cd $(TMP); mkdir -p repo
	cd $(TMP); tar -czvf repo/lib01-0.1.0.0.tar.gz "lib01-0.1.0.0"
	cd $(TMP); cp -r $(PDIR)/lib-0.2 "lib01-0.2.0.0"
	cd $(TMP); mkdir -p repo
	cd $(TMP); tar -czvf repo/lib01-0.2.0.0.tar.gz "lib01-0.2.0.0"
	#tar -czvf repo/lib01-0.2.0.0.tar.gz lib-0.2 \
	ls $(TMP)
	mv "$(TMP)/repo" .
	$(CABAL) update

build:
	$(CABAL) build all

run: build
	$(CABAL) build exe:hooks-exe --constraint="hooks-exe:exe:hooks-exe.lib01 == 0.1.0.0"
	$(eval HOOKS_EXE := $(shell $(CABAL) list-bin exe:hooks-exe))
	PATH=$(shell dirname $(HOOKS_EXE)):$$PATH $(CABAL) run exe:main-prog

	$(CABAL) build exe:hooks-exe --constraint="hooks-exe:exe:hooks-exe.lib01 == 0.2.0.0"
	$(eval HOOKS_EXE := $(shell $(CABAL) list-bin exe:hooks-exe))
	PATH=$(shell dirname $(HOOKS_EXE)):$$PATH $(CABAL) run exe:main-prog



