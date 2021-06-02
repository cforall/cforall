AM_T_CFA = $(am__t_CFA_@AM_T@)
am__t_CFA_ =
am__t_CFA_0 =
am__t_CFA_1 = /usr/bin/time --quiet -f "$@ %E" # trailling space is necessary


CFACOMPILE = $(AM_T_CFA)$(CFACC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CFAFLAGS) $(CFAFLAGS) $(AM_CFLAGS) $(CFLAGS)
LTCFACOMPILE = $(AM_T_CFA)$(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=compile $(CFACC) $(DEFS) \
	$(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CFAFLAGS) $(AM_CFLAGS) $(CFAFLAGS) $(CFLAGS)

AM_V_CFA = $(am__v_CFA_@AM_V@)
am__v_CFA_ = $(am__v_CFA_@AM_DEFAULT_V@)
am__v_CFA_0 = @echo "  CFA     " $@;
am__v_CFA_1 =

.cfa.o:
	$(AM_V_CFA)depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`;\
	$(CFACOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
	$(am__mv) $$depbase.Tpo $$depbase.Po

.cfa.lo:
	$(AM_V_CFA)depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.lo$$||'`;\
	$(LTCFACOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
	$(am__mv) $$depbase.Tpo $$depbase.Plo

UPPCC = u++
UPPCOMPILE = $(UPPCC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_UPPFLAGS) $(UPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) $(AM_CFLAGS) $(CFLAGS)

AM_V_UPP = $(am__v_UPP_@AM_V@)
am__v_UPP_ = $(am__v_UPP_@AM_DEFAULT_V@)
am__v_UPP_0 = @echo "  UPP     " $@;
am__v_UPP_1 =

AM_V_GOC = $(am__v_GOC_@AM_V@)
am__v_GOC_ = $(am__v_GOC_@AM_DEFAULT_V@)
am__v_GOC_0 = @echo "  GOC     " $@;
am__v_GOC_1 =

AM_V_PY = $(am__v_PY_@AM_V@)
am__v_PY_ = $(am__v_PY_@AM_DEFAULT_V@)
am__v_PY_0 = @echo "  PYTHON  " $@;
am__v_PY_1 =

AM_V_RUST = $(am__v_RUST_@AM_V@)
am__v_RUST_ = $(am__v_RUST_@AM_DEFAULT_V@)
am__v_RUST_0 = @echo "  RUST    " $@;
am__v_RUST_1 =

AM_V_NODEJS = $(am__v_NODEJS_@AM_V@)
am__v_NODEJS_ = $(am__v_NODEJS_@AM_DEFAULT_V@)
am__v_NODEJS_0 = @echo "  NODEJS  " $@;
am__v_NODEJS_1 =

AM_V_JAVAC = $(am__v_JAVAC_@AM_V@)
am__v_JAVAC_ = $(am__v_JAVAC_@AM_DEFAULT_V@)
am__v_JAVAC_0 = @echo "  JAVAC   " $@;
am__v_JAVAC_1 =
