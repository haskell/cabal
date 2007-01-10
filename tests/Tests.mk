
# Some hacks
HC = $(shell grep "^HC=" ../../Makefile | sed "s/HC=//")
comma = ,
CABALVERSION = $(shell grep "^CABALVERSION=" ../../Makefile | sed "s/CABALVERSION=//")

setup: Setup.lhs
	$(HC) -cpp -DCABAL_VERSION=$(subst .,$(comma),$(CABALVERSION)) --make -i../.. Setup.lhs -o setup 2>out.build
clean:
	rm -f setup a.out .setup-config register.sh unregister.sh out.build
	rm -rf ,tmp* dist
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	find . -name "*~" | xargs rm -f
check: setup
	./setup configure --user --prefix=/tmp/foo
	./setup build
	./setup install --install-prefix=/tmp/bar
	ls /tmp/bar*
	# install w/ register!
	./setup install
#	ls /tmp/foo*
	./setup sdist
	ls dist
