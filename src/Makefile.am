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
## Created On       : Sun May 31 08:51:46 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Mon Aug  5 12:57:46 2019
## Update Count     : 98
###############################################################################

# create object files in directory with source files
AUTOMAKE_OPTIONS = foreign subdir-objects
ACLOCAL_AMFLAGS  = -I automake

SRC = main.cc \
      CompilationState.cc \
      CompilationState.h \
      MakeLibCfa.cc \
	  MakeLibCfaNew.cpp \
	MakeLibCfa.h

SRCDEMANGLE = CompilationState.cc

MAINTAINERCLEANFILES =
MOSTLYCLEANFILES =

if WITH_LIBPROFILER
LIBPROFILER = -lprofiler
endif

if WITH_LIBTCMALLOC
LIBTCMALLOC = -ltcmalloc
TCMALLOCFLAG = -DTCMALLOC
endif

include AST/module.mk
include CodeGen/module.mk
include CodeTools/module.mk
include Concurrency/module.mk
include Common/module.mk
include ControlStruct/module.mk
include GenPoly/module.mk
include InitTweak/module.mk
include Parser/module.mk
include ResolvExpr/module.mk
include SymTab/module.mk
include SynTree/module.mk
include Tuples/module.mk
include Validate/module.mk
include Virtual/module.mk

$(addprefix $(srcdir)/, ResolvExpr/ConversionCost.cc ResolvExpr/CommonType.cc SymTab/ManglerCommon.cc) : $(srcdir)/SynTree/Type.h

$(srcdir)/AST/Type.hpp : BasicTypes-gen.cc
	${AM_V_GEN}${CXXCOMPILE} $< -o BasicTypes-gen -Wall -Wextra -Werror=return-type
	@./BasicTypes-gen
	@rm BasicTypes-gen

# put into lib for now
cfa_cpplibdir = $(CFA_LIBDIR)
cfa_cpplib_PROGRAMS = ../driver/cfa-cpp $(DEMANGLER)
EXTRA_PROGRAMS = demangler
___driver_cfa_cpp_SOURCES = $(SRC)
___driver_cfa_cpp_LDADD = -ldl $(LIBPROFILER) $(LIBTCMALLOC)
EXTRA_DIST = include/cassert include/optional BasicTypes-gen.cc

AM_CXXFLAGS = @HOST_FLAGS@ -Wno-deprecated -Wall -Wextra -Werror=return-type -DDEBUG_ALL -I./Parser -I$(srcdir)/Parser -I$(srcdir)/include -DYY_NO_INPUT -O3 -g -std=c++14 $(TCMALLOCFLAG)
AM_LDFLAGS  = @HOST_FLAGS@ -Xlinker -export-dynamic
ARFLAGS     = cr

demangler_SOURCES = SymTab/demangler.cc # test driver for the demangler, also useful as a sanity check that libdemangle.a is complete

demangler_LDADD = libdemangle.a -ldl			# yywrap

noinst_LIBRARIES = $(LIBDEMANGLE)
EXTRA_LIBRARIES = libdemangle.a
libdemangle_a_SOURCES = $(SRCDEMANGLE)

MAINTAINERCLEANFILES += ${libdir}/${notdir ${cfa_cpplib_PROGRAMS}}
