all:
	mkdir -p dist/tmp
	ghc -Wall --make -odir dist/tmp -hidir dist/tmp -i../..:../HUnit-1.0/src Setup.lhs -o setup
clean:
	rm -f setup a.out dist/build/testA
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
