all: tests

tests:
	ghc -Wall --make -i../:/usr/local/src/HUnit-1.0 Distribution/ModuleTest -o moduleTest

check: tests
	./moduleTest

pushall:
	darcs push --all ijones@monk.syntaxpolice.org:/home/ijones/public_html/darcs_repos/library-infrastructure
	darcs push --all ijones@cvs.haskell.org:/home/ijones/library-infrastructure
