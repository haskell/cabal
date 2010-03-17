TOP = ../../..

ifeq "$(findstring boilerplate.mk, $(wildcard $(TOP)/mk/*))" ""
# ----------------------------------------------------------------------------
# Standalone Makefile:

.PHONY: all

all: Cabal.pdf

Cabal.pdf: Cabal.xml
	docbook2pdf Cabal.xml

clean:
	rm -fr *~ API users-guide Cabal.pdf Cabal.dvi semantic.cache

else # boilerplate.mk exists
# ----------------------------------------------------------------------------
# GHC build tree Makefile:

include $(TOP)/mk/boilerplate.mk

DOC_SUBDIR=libraries/Cabal/doc
XML_DOC = Cabal
INSTALL_XML_DOC = $(XML_DOC)

binary-dist:
	@:

include $(TOP)/mk/target.mk

endif
