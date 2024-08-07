######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Richard C. Bilson
## Created On       : Mon Jun  1 17:49:17 2015
## Last Modified By : Andrew Beach
## Last Modified On : Tue May 17 14:46:00 2022
## Update Count     : 5
###############################################################################

SRC_SYMTAB = \
	SymTab/FixFunction.cpp \
	SymTab/FixFunction.hpp \
	SymTab/GenImplicitCall.cpp \
	SymTab/GenImplicitCall.hpp \
	SymTab/Mangler.cpp \
	SymTab/ManglerCommon.cpp \
	SymTab/Mangler.hpp

SRC += $(SRC_SYMTAB)

SRCDEMANGLE += $(SRC_SYMTAB) \
	SymTab/Demangle.cpp \
	SymTab/Demangle.hpp
