TOP = ../..

ifeq "$(findstring boilerplate.mk, $(wildcard $(TOP)/mk/*))" ""
# ----------------------------------------------------------------------------
# Standalone Makefile:

all: Cabal.xml
	docbook2pdf Cabal.xml

clean:
	rm -fr *~ API users-guide Cabal.pdf Cabal.dvi semantic.cache

else # boilerplate.mk exists
# ----------------------------------------------------------------------------
# GHC build tree Makefile:

include $(TOP)/mk/boilerplate.mk

XML_DOC = Cabal
INSTALL_XML_DOC = $(XML_DOC)

include $(TOP)/mk/target.mk

endif
