all:
	mkdir -p dist/tmp
	ghc --make -odir dist/tmp -hidir dist/tmp -i../.. Setup.lhs -o setup
semiclean:
	rm -rf ,tmp dist
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
clean: semiclean
	rm -f setup a.out hunitTest
	rm -f installed-pkg-config

check: all
	./setup configure --user --prefix=/tmp/foo
	./setup install --install-prefix=/tmp/bar
	ls /tmp/bar*
	# install w/ register!
	./setup install
#	ls /tmp/foo*
	./setup sdist
	ls dist
