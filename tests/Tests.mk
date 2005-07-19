#HC=/tmp/ghc/bin/ghc
HC=ghc
setup: Setup.lhs
	$(HC) -cpp --make -i../.. Setup.lhs -o setup 2>out.build
clean:
	rm -f setup a.out .setup-config register.sh unregister.sh out.build
	rm -rf ,tmp* dist
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	find . -name "*~" | xargs rm -f
check: setup
	./setup configure --user --prefix=/tmp/foo
	./setup build
	./setup install --install-prefix=/tmp/bar
	ls /tmp/bar*
	# install w/ register!
	./setup install
#	ls /tmp/foo*
	./setup sdist
	ls dist
