all:
	ghc -Wall --make -i../:/usr/local/src/HUnit-1.0 Setup.hs -o setup
clean:
	rm *.o setup *.hi
check: all
	./setup configure --prefix=/tmp/foo
	./setup install
	ls /tmp/foo*
	./setup sdist
	ls dist
