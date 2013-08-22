# Makefile for various compilations of the system libraries,
# in particular, to generate the documentation

CYMAKEPARAMS = --extended --no-verb --no-warn --no-overlap-warn -i. -imeta

# directory for HTML documentation files
# LIBDOCDIR = $(DOCDIR)/html
LIBDOCDIR := CDOC
# directory for LaTeX documentation files
TEXDOCDIR := $(DOCDIR)/src/lib

# replacement stuff
comma     := ,
empty     :=
space     := $(empty) $(empty)
# dir/file.ext -> dir/.curry/file.ext
add_subdir = $(dir $(file)).curry/$(notdir $(file))
# a b c -> a, b, c
comma_sep  = $(subst $(space),$(comma)$(space),$(1))

LIB_CURRY     = $(filter-out $(EXCLUDES), $(wildcard *.curry meta/*.curry))
LIB_FCY       = $(foreach file, $(LIB_CURRY:.curry=.fcy), $(add_subdir))
LIB_ACY       = $(foreach file, $(LIB_CURRY:.curry=.acy), $(add_subdir))
# lib names without directory prefix
LIB_NAMES     = $(basename $(notdir $(LIB_CURRY)))
LIB_HTML      = $(foreach lib, $(LIB_NAMES), $(LIBDOCDIR)/$(lib).html)
LIB_TEX       = $(foreach lib, $(LIB_NAMES), $(TEXDOCDIR)/$(lib).tex)
HS_LIB_NAMES  = $(call comma_sep,$(LIB_NAMES:%=Curry_%))

ALLLIBS       = AllLibraries.curry
EXCLUDES      = $(ALLLIBS) Curry_Main_Goal.curry

PACKAGE       = kics2-libraries
CABAL_FILE    = $(PACKAGE).cabal
CABAL_LIBDEPS = $(call comma_sep,$(LIBDEPS))

########################################################################
# support for installation
########################################################################

.PHONY: install
install: $(CABAL_FILE)
	$(MAKE) compile
	$(CABAL_INSTALL)

# compile all libraries
.PHONY: compile
compile: $(ALLLIBS)
	"$(REPL)" $(REPL_OPTS) :set path $(LIBDIR):$(LIBDIR)/meta :l $< :eval main :quit

.PHONY: all
all: fcy acy

.PHONY: fcy
fcy: $(LIB_FCY)

.PHONY: acy
acy: $(LIB_ACY)

.PHONY: unregister
unregister:
	-$(GHC_UNREGISTER) $(PACKAGE)-$(VERSION)

# clean all generated files
.PHONY: cleanall
cleanall:
	rm -rf "$(LIBDOCDIR)"
	rm -rf "$(TEXDOCDIR)"
	rm -rf dist
	rm -f $(CABAL_FILE)
	$(CLEANCURRY)
	cd meta && $(CLEANCURRY)

$(ALLLIBS): $(LIB_CURRY) Makefile
	rm -f $@
	for i in $(LIB_NAMES) ; do echo "import $$i" >> $@ ; done
	echo "main = 42" >> $@

$(CABAL_FILE): ../Makefile Makefile
	echo "Name:           $(PACKAGE)"                             > $@
	echo "Version:        $(VERSION)"                            >> $@
	echo "Description:    The standard libraries for KiCS2"      >> $@
	echo "License:        OtherLicense"                          >> $@
	echo "Author:         Fabian Reck"                           >> $@
	echo "Maintainer:     fre@informatik.uni-kiel.de"            >> $@
	echo "Build-Type:     Simple"                                >> $@
	echo "Cabal-Version:  >= 1.9.2"                              >> $@
	echo ""                                                      >> $@
	echo "Library"                                               >> $@
	echo "  Build-Depends:"                                      >> $@
	echo "      kics2-runtime == $(VERSION)"                     >> $@
	echo "    , $(CABAL_LIBDEPS)"                                >> $@
	echo "  if os(windows)"                                      >> $@
	echo "    Build-Depends: Win32"                              >> $@
	echo "  else"                                                >> $@
	echo "    Build-Depends: unix"                               >> $@
	echo "  Exposed-modules: $(HS_LIB_NAMES)"                    >> $@
	echo "  hs-source-dirs: ./.curry/kics2, ./meta/.curry/kics2" >> $@

# generate all FlatCurry files in subdirectory .curry
.curry/%.fcy: %.curry
	"${CYMAKE}" --flat ${CYMAKEPARAMS} $*

meta/.curry/%.fcy: meta/%.curry
	"${CYMAKE}" --flat ${CYMAKEPARAMS} $*

# generate all AbstractCurry files in subdirectory .curry
.curry/%.acy: %.curry
	"${CYMAKE}" --acy ${CYMAKEPARAMS} $*

meta/.curry/%.acy: meta/%.curry
	"${CYMAKE}" --acy ${CYMAKEPARAMS} $*

##############################################################################
# create HTML documentation files for system libraries
##############################################################################

.PHONY: doc
doc: $(LIB_CURRY)
	mkdir -p "$(LIBDOCDIR)"
	$(MAKE) $(LIB_HTML)
	@echo "Generating index pages for Curry libraries:"
	@echo $(LIB_NAMES)
	"$(CURRYDOC)" --onlyindexhtml "$(LIBDOCDIR)" $(LIB_NAMES)

# generate individual documentations for libraries
$(LIBDOCDIR)/%.html: %.curry
	"$(CURRYDOC)" --noindexhtml "$(LIBDOCDIR)" $*

$(LIBDOCDIR)/%.html: meta/%.curry
	"$(CURRYDOC)" --noindexhtml "$(LIBDOCDIR)" $*

##############################################################################
# create LaTeX documentation files for system libraries
##############################################################################

.PHONY: texdoc
texdoc: $(LIB_CURRY)
	mkdir -p "$(TEXDOCDIR)"
	$(MAKE) $(LIB_TEX)

# generate individual LaTeX documentations for libraries
$(TEXDOCDIR)/%.tex: %.curry
	"$(CURRYDOC)" --tex "$(TEXDOCDIR)" $*

$(TEXDOCDIR)/%.tex: meta/%.curry
	"$(CURRYDOC)" --tex "$(TEXDOCDIR)" $*
