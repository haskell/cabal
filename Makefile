TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = doc

ALL_DIRS = \
	Distribution \
	Distribution/Simple \
	Distribution/PreProcess \
	Distribution/Compat \
	Language/Haskell

EXCLUDED_SRCS = DefaultSetup.lhs

PACKAGE		= Cabal
VERSION		= 1.1.4

PACKAGE_DEPS	= base

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (Cabal package)"

SRC_HC_OPTS   += -cpp

include $(TOP)/mk/target.mk
