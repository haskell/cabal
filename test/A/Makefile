all:
	ghc -Wall --make -i../../:/usr/local/src/HUnit-1.0 Setup.lhs -o setup
clean:
	rm -f setup a.out
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
check: all
	./setup configure --user --prefix=/tmp/foo
	./setup build
	./setup install --install-prefix=/tmp/bar
	ls /tmp/bar*
	# install w/ register!
	./setup install
#	ls /tmp/foo*
	./setup sdist
	ls dist
