# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk

#
# Disable 'make boot'
#
NO_BOOT_TARGET=YES

WAYS=

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.hs)
BINS        := $(EXAMPLES:.hs=$(exeext))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -package Cabal

all:: $(BINS)

$(BINS): %$(exeext): %.hs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
