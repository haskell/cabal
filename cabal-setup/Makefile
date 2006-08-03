TOP=../..

ifneq "$(findstring boilerplate.mk, $(wildcard $(TOP)/mk/*))" ""

# ----------------------------------------------------------------------------
# GHC build tree Makefile:

include $(TOP)/mk/boilerplate.mk

HC		= $(GHC_INPLACE)
MKDEPENDHS      = $(GHC_INPLACE)
HS_PROG 	= cabal-setup

EXCLUDED_SRCS 	= Setup.hs
SRC_HC_OPTS 	+= -package Cabal

INSTALL_PROGS	+= $(HS_PROG)

include $(TOP)/mk/target.mk

endif
