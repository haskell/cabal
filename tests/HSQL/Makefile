include config.mk

COMPILERS =
ifneq "$(GHC)" ""
COMPILERS += ghc
endif
# ifneq "$(HUGS)" ""
# COMPILERS += hugs
# endif

#--------------------------------------------------------------------------
# DIRECTORIES
#--------------------------------------------------------------------------

# library and doc output
BUILDDIR  = build
DOCDIR	  = doc

HOUTDIR = $(BUILDDIR)/Database

HSQLLib = $(BUILDDIR)/libHSsql.a

# source dirs
VPATH = src

#--------------------------------------------------------------------------
# SOURCES
#--------------------------------------------------------------------------

HSC_SRC += HSQL.hsc

ifeq "$(WithODBC)" "YES"
HSC_SRC += HSQL/ODBC.hsc
endif

ifeq "$(WithPostgreSQL)" "YES"
HSC_SRC += HSQL/PostgreSQL.hsc
endif

ifeq "$(WithMySQL)" "YES"
HSC_SRC += HSQL/MySQL.hsc
endif

ifeq "$(WithSQLite)" "YES"
HSC_SRC += HSQL/SQLite.hsc
endif

SO_SRC  = $(patsubst %.hsc,$(HOUTDIR)/%.$(SO_EXT),$(HSC_SRC))
HS_SRC  = $(HOUTDIR)/HSQL/Types.hs $(patsubst %.hsc,$(HOUTDIR)/%.hs,$(HSC_SRC))
HI_SRC  = $(patsubst %.hs,%.hi,$(HS_SRC))
OBJS    = $(patsubst %.hs,%.o, $(HS_SRC))
HS_DEPS = $(patsubst %.hs,%.d, $(HS_SRC))

ifeq "$(WithODBC)" "YES"
ifeq "$(WIN32)" "YES"
HsODBC_o = $(HOUTDIR)/HSQL/HsODBC.o
endif
endif

#--------------------------------------------------------------------------
# FLAGS
#--------------------------------------------------------------------------

FFIHUGS_FLAGS = $(patsubst %, +L"%", $(CPPFLAGS)) 
ifeq "$(WIN32)" "YES"
ODBC_FFIHUGS_FLAGS = +L"$(HsODBC_o)"
FFIHUGS_FLAGS += $(patsubst %, +L"%", $(patsubst -l%, %.lib, $(LDFLAGS)))
else
FFIHUGS_FLAGS += $(patsubst %, +L"%", $(LDFLAGS))
endif

ifeq "$(HADDOCK)" ""
HADDOCK = echo
else
HS_PPS = $(addsuffix .raw-hs, $(basename $(HS_SRC)))
endif

#--------------------------------------------------------------------------
# RULES
#--------------------------------------------------------------------------

.SUFFIXES: .hs .hi .o .c

# should be:
#
#     $(HSC2HS) $< -o $@ $(CPPFLAGS)
#
# but there is a bug in hsc2hs from GHC 6.2 for Windows
$(HOUTDIR)/%.hs: %.hsc
	$(HSC2HS) $< $(CPPFLAGS)
	mv $(patsubst %.hsc, %.hs, $<) $@
%.o : %.hs
	mkdir -p $(basename $<)_split
	rm -f $(basename $<)_split/*
	$(GHC) $< -O -c -i$(BUILDDIR) -fglasgow-exts -split-objs $(CPPFLAGS) -package-name hsql
	$(LD) -r -o $@ $(basename $<)_split/*.o
	@# create dependency file
	@$(GHC) $< -M -i$(BUILDDIR) -optdep-f -optdep$(*F).d $(CPPFLAGS)
	@sed -e 's|$(subst .hs,,$<)\.o|$*\.o|' $(*F).d > $*.d
	@rm $(*F).d
.o.hi:
	@:
%.$(SO_EXT): %.hs
	$(FFIHUGS) -98 +G -P$(BUILDDIR): $(FFIHUGS_FLAGS) $($(*F)_FFIHUGS_FLAGS) $<
%.raw-hs : %.hs
	$(GHC) -Iincludes $(CPPFLAGS) -D__HADDOCK__ -E -cpp $< -o $<.tmp && sed -e 's/^#.*//' <$<.tmp >$@


all: $(patsubst %, all-%, $(COMPILERS))
all-ghc: $(HSQLLib)
all-hugs: $(SO_SRC)


$(HSQLLib): $(OBJS) $(HsODBC_o)
	rm -f $(HSQLLib)
	for i in $(patsubst %.o,%_split, $(OBJS)); do \
	   $(AR) -q $(HSQLLib) $$i/*.o; \
	done
	$(AR) -q $(HSQLLib) $(HsODBC_o)

genclean: distclean
	rm -rf configure autom4te.cache

distclean: clean
	rm -f $(HSQLLib)
	rm -f config.status config.log config.mk hsql.pkg

clean:
	rm -f $(OBJS)
	rm -f $(HI_SRC)
	rm -f $(SO_SRC)
	rm -f $(HS_SRC)
	rm -f $(patsubst %.hs,%.c,$(HS_SRC))
	rm -f $(HS_PPS)
	rm -f $(HS_DEPS)
	rm -rf $(patsubst %.o,%_split, $(OBJS))
	rm -f $(HSQLLib)


docs : $(HS_PPS)
	mkdir -p $(DOCDIR)
	$(HADDOCK) -h -o $(DOCDIR) $(HS_PPS)


install: $(patsubst %, install-%, $(COMPILERS))

install-ghc: $(HSQLLib) $(HI_SRC) docs
	$(INSTALL) -d $(GHC_DIR)
	$(INSTALL) $(HSQLLib) $(GHC_DIR)/libHSsql.a
	for i in $(patsubst $(BUILDDIR)/%,%,$(HI_SRC)); do \
	   $(INSTALL) -d $(GHC_DIR)/imports/`dirname $$i`; \
	   $(INSTALL) -c $(BUILDDIR)/$$i $(GHC_DIR)/imports/`dirname $$i`; \
	done
	rm -f $(GHC_DIR)/HSsql.o
	$(GHC_PKG) --config-file=tmpConfig -u -g -i hsql.pkg
	if test -f $(DOCDIR)/index.html; then  \
	   $(INSTALL) -d $(DOC_DIR); \
	   $(INSTALL) -c $(DOCDIR)/* $(DOC_DIR); \
	fi

install-hugs: $(HS_SRC) $(SO_SRC)
	$(INSTALL) -d $(HUGS_DIR)/libraries/Database
	for i in $(patsubst $(BUILDDIR)/%,%,$(HS_SRC) $(SO_SRC)); do \
	   $(INSTALL) -d $(HUGS_DIR)/libraries/`dirname $$i`; \
	   $(INSTALL) -c $(BUILDDIR)/$$i $(HUGS_DIR)/libraries/`dirname $$i`; \
	done

setup: Setup.lhs
	ghc -cpp --make -i../.. Setup.lhs -o setup 2>out.build

-include $(HS_DEPS)

$(HOUTDIR)/HSQL/ODBC.hs : HSQL/HsODBC.h

$(HOUTDIR)/HSQL/Types.hs : HSQL/Types.hs
	cp $< $@

$(HOUTDIR)/HSQL/HsODBC.o : HSQL/HsODBC.c HSQL/HsODBC.h
	$(CC) $< -o $@ -O -c $(CPPFLAGS)
