all:
	ghc -Wall --make -i../../:/usr/local/src/HUnit-1.0 Setup.lhs -o setup
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
