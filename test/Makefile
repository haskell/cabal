all:
	ghc -Wall --make -i../:/usr/local/src/HUnit-1.0 Setup.hs -o setup
clean:
	rm -f setup a.out
	rm -rf ,tmp dist
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
check: all
	./setup configure --user --prefix=/tmp/foo
	./setup install --install-prefix=/tmp/bar
	ls /tmp/bar*
	# install w/ register!
	./setup install
#	ls /tmp/foo*
	./setup sdist
	ls dist
