######################## -*- Mode: Makefile-Automake -*- ######################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## Makefile.am --
##
## Author           : Peter A. Buhr
## Created On       : Sun May 31 08:49:31 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Thu Aug  2 12:18:25 2018
## Update Count     : 14
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

# applies to both programs
AM_CXXFLAGS = @HOST_FLAGS@ -Wall -O2 -g -std=c++14 -I${abs_top_srcdir}/src -I${abs_top_srcdir}/src/include

# don't install cfa directly
noinst_PROGRAMS = cfa

# use
install-exec-hook:
	@test -z "$(CFA_BINDIR)" || $(MKDIR_P) "$(CFA_BINDIR)"
	@echo " $(INSTALL_PROGRAM_ENV) $(INSTALL_PROGRAM) cfa '$(CFA_BINDIR)/$(CFA_NAME)'"; \
	chmod u+w $(CFA_BINDIR);\
	$(INSTALL_PROGRAM_ENV) $(INSTALL_PROGRAM) cfa $(CFA_BINDIR)/$(CFA_NAME) || exit $$?

uninstall-hook:
	@echo " ( cd '$(CFA_BINDIR)' && rm -f $(CFA_NAME) )"; \
	cd "$(CFA_BINDIR)" && rm -f $(CFA_NAME)

cfa_SOURCES = cfa.cc

# put into lib for now
cc1libdir = ${CFA_LIBDIR}
cc1lib_PROGRAMS = as cc1
as_SOURCES = as.cc
cc1_SOURCES = cc1.cc

MAINTAINERCLEANFILES = $(CFA_BINDIR)/$(CFA_NAME) @CFA_PREFIX@/lib/${cc1lib_PROGRAMS}
