
# type in sources here
SOURCES = options.ml settings.ml
MLI = options.mli settings.mli

include $(top_srcdir)/m4/Makefile.am.vars

# overwrite some variables

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets

install-data-am: $(TARGET)
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" != ""; then \
	    $(mkinstalldirs) $(SITELIBDIR); \
	    $(OCAMLFIND) install -destdir $(SITELIBDIR) $(PACKAGE) $(TARGET); \
	else \
	    $(OCAMLFIND) install $(PACKAGE) $(TARGET); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    installdir=$(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    installdir=$(SITELIBDIR)/$(PACKAGE); \
	fi; \
	$(mkinstalldirs) $$installdir; \
	for i in $(TARGET); do \
	    $(INSTALL_DATA) $$i $$installdir/$$i; \
	done
endif

#install-exec-am: $(BYTE) $(NATIVE)
#	$(mkinstalldirs) ${bindir}
#	$(INSTALL_PROGRAM) $(BYTE) ${bindir}/$(BYTE)
#	$(INSTALL_PROGRAM) $(NATIVE) ${bindir}/$(PACKAGE)$(EXEEXT)

uninstall-am:
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" = ""; then \
	    $(OCAMLFIND) remove $(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    rm -fR $(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
endif
#	rm -f ${bindir}/$(BYTE) ${bindir}/$(PACKAGE)$(EXEEXT)

include $(top_srcdir)/m4/Makefile.am.rules
