all:
	mkdir -p dist/tmp
	ghc --make -odir dist/tmp -hidir dist/tmp -i../.. Setup.lhs -o setup
clean:
	rm -f setup
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
