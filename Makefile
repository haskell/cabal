all: tests

tests:
	ghc -Wall --make -i../:/usr/local/src/HUnit-1.0 Distribution/ModuleTest -o moduleTest

check: tests
	./moduleTest