all: main

main:
	ghc -Wall --make -i../:/usr/local/src/HUnit-1.0 Distribution/ModuleTest -o moduleTest
tests:
	cd test/A && make

clean:
	-rm -f Distribution/*.{o,hi} Distribution/Simple/*.{o,hi} 

check: tests
	./moduleTest

pushall:
	darcs push --all ijones@monk.syntaxpolice.org:/home/ijones/public_html/darcs_repos/library-infrastructure
	darcs push --all ijones@cvs.haskell.org:/home/ijones/library-infrastructure

dist: pushall
	darcs dist
	scp library-infrastructure--darcs.tar.gz ijones@www.haskell.org:~/libraryInfrastructure/libraryInfrastructure-code.tgz
