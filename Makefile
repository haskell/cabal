TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Distribution \
	Distribution/Simple \
	Distribution/PreProcess \
	Distribution/Compat

PACKAGE		= Cabal
VERSION		= 1.0

# Distribution.PackageDescription requires mtl for the Error monad,
PACKAGE_DEPS	= base mtl

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (Cabal package)"

include $(TOP)/mk/target.mk
