setup:
	ghc -cpp --make -i../.. Setup.lhs -o setup 2>out.build
clean:
	rm -f setup a.out .setup-config
	rm -rf ,tmp* dist
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	find . -name "*~" | xargs rm -f
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
