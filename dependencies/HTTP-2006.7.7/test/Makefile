GHC = ghc
GHCFLAGS = -O2 -package HTTP

TEST_PROGS = get

.SUFFIXES: .hs .hi .o

.PHONY: all clean

default all: $(TEST_PROGS)

%: %.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

clean:
	-rm -f *.hi *.o $(TEST_PROGS)
