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
## Last Modified On : Mon Nov 20 11:28:05 2023
## Update Count     : 109
###############################################################################

# create object files in directory with source files
AUTOMAKE_OPTIONS = foreign subdir-objects
ACLOCAL_AMFLAGS  = -I automake

SRC = main.cpp \
	CompilationState.cpp \
	CompilationState.hpp \
	MakeLibCfa.cpp \
	MakeLibCfa.hpp

SRCDEMANGLE = CompilationState.cpp

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
include Concurrency/module.mk
include Common/module.mk
include ControlStruct/module.mk
include GenPoly/module.mk
include InitTweak/module.mk
include Parser/module.mk
include ResolvExpr/module.mk
include SymTab/module.mk
include Tuples/module.mk
include Validate/module.mk
include Virtual/module.mk

$(addprefix $(srcdir)/, ResolvExpr/ConversionCost.cpp ResolvExpr/CommonType.cpp SymTab/ManglerCommon.cpp) : $(srcdir)/AST/BasicKind.hpp

$(srcdir)/AST/BasicKind.hpp : BasicTypes-gen.cpp
	${AM_V_GEN}${CXXCOMPILE} $< -o BasicTypes-gen -Wall -Wextra -Werror=return-type
	@./BasicTypes-gen
	@rm BasicTypes-gen

# put into lib for now
cfa_cpplibdir = $(CFA_LIBDIR)
cfa_cpplib_PROGRAMS = ../driver/cfa-cpp
___driver_cfa_cpp_SOURCES = $(SRC)
___driver_cfa_cpp_LDADD = -ldl $(LIBPROFILER) $(LIBTCMALLOC)
EXTRA_DIST = include/cassert include/optional BasicTypes-gen.cpp

AM_CXXFLAGS = @HOST_FLAGS@ -Wno-deprecated -Wall -Wextra -Werror=return-type -DDEBUG_ALL -I./Parser -I$(srcdir)/Parser -I$(srcdir)/include -DYY_NO_INPUT -O3 -g -std=c++17 $(TCMALLOCFLAG)
AM_LDFLAGS  = @HOST_FLAGS@ -Xlinker -export-dynamic
ARFLAGS     = cr

cfa_cpplib_PROGRAMS += $(DEMANGLER)
EXTRA_PROGRAMS = ../driver/demangler
___driver_demangler_SOURCES = SymTab/demangler.cpp # test driver for the demangler, also useful as a sanity check that libdemangle.a is complete
___driver_demangler_LDADD = libdemangle.a -ldl			# yywrap
noinst_LIBRARIES = $(LIBDEMANGLE)
EXTRA_LIBRARIES = libdemangle.a
libdemangle_a_SOURCES = $(SRCDEMANGLE)

MAINTAINERCLEANFILES += ${libdir}/${notdir ${cfa_cpplib_PROGRAMS}}
