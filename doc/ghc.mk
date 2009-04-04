
libraries/Cabal/doc_DOCBOOK_SOURCES := $(wildcard libraries/Cabal/doc/*.xml)

$(eval $(call docbook,libraries/Cabal/doc,Cabal))

